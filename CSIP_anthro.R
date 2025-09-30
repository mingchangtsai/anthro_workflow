library(pacman)
p_load(tidyverse,smartabaseR,conflicted,here)
suppressMessages(conflict_prefer_all("dplyr", quiet = T))

## https://teamworksapp.github.io/smartabaseR/index.html
cat(as.character(Sys.time()),"\n")

filelist <- list.files(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","CSIP","anthro","Documents - Anthropometry","upload_data"), pattern="csv", full.names = T)
namelist <- suppressWarnings(suppressMessages(read_csv(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","CSIP","anthro","Documents - Anthropometry","reference","full_namelist.csv")))) %>% 
  unite("About","First Name","Last Name", sep = " ") %>% 
  rename(user_id=UUID) %>% 
  select(About,user_id)
reporter <- suppressMessages(read_csv(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","CSIP","anthro","Documents - Anthropometry","reference","reporter.csv")))

Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools")
### insert event
if(!is_empty(filelist)){
  for(i in seq_along(filelist)){
    testdata <- suppressMessages(read_csv(filelist[i])) %>% 
      janitor::remove_empty("cols") %>%
      rename(start_date=Date) %>% 
      # mutate(start_date=as.character(start_date)) %>% 
      mutate(start_date=gsub("-","/",start_date)) %>%
      left_join(namelist,by="About") %>% 
      filter(!is.na(About))
    
    no_user <- testdata$About[is.na(testdata$user_id)]
    to <- reporter$email[reporter$reporter==testdata$`Name of Data Reporter`[1]]
    if(length(no_user)>0){
      # testdata <- testdata %>% filter(!is.na(user_id))
      source(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","CSIP","anthro","code","error_mail.R"))
      cat("error email sent!\n")
    } else {
      source(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","CSIP","anthro","code","sb_upload.R"))
      source(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","CSIP","anthro","code","report_generation.R"))
      source(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","CSIP","anthro","code","mail_report.R"))
      
      cat("data uploaded!\n")
    }
    file.copy(filelist[i],gsub("upload_data/",paste0("upload_data/archived/",Sys.Date(),"_"),filelist[i]))
    file.remove(filelist[i])
  }
} else {
  cat("no files found!\n")
}


# library(cronR)
# cmd1 <- cron_rscript("CSIP_anthro.R")
# cron_add(cmd1, frequency = "*/5 * * * *", id="CSIP_anthro", description="anthro data upload, checking every 5min")

