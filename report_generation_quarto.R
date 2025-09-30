library(pacman)
p_load(tidyverse,readxl,lubridate,conflicted,rmarkdown,knitr,here,quarto)
suppressMessages(conflict_prefer("filter","dplyr"))

# source("sb_download.R")
source(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","CSIP","anthro","code","sb_download.R"))
anthro <- mydata %>% 
  rename(Date=start_date) %>% 
  mutate(Date=dmy(Date)) %>%
  arrange(desc(Date)) %>% 
  # filter(!grepl("est|Swimmer",about)) %>% 
  unite("AthleteName",first_name,last_name, sep = " ") %>% 
  distinct()

# a <- sort(unique(anthro$AthleteName))
# b=30
# Sys.setenv(RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools")

for(b in 1:nrow(testdata)){
  AthleteData <- anthro %>% 
    filter(about==testdata$About[b]) %>% 
    select(Date,AthleteName,`Name of Data Reporter`,Weight,`Lean Mass Index`,`Sum of 8 Skinfolds`,
           matches("(Mid Thigh|Medial Calf|Bicep|Waist|Hip).*Mean"),
           -matches("Girth|R Bicep Mean|L Bicep Mean")) %>%
    rename(reporter=`Name of Data Reporter`,
           lmi=`Lean Mass Index`,
           so8=`Sum of 8 Skinfolds`,
           r_calf=`R Medial Calf Mean`,
           r_relax_bi=`R Relaxed Bicep Mean`,
           r_flex_bi=`R Flexed Bicep Mean`,
           l_relax_bi=`L Relaxed Bicep Mean`,
           l_flex_bi=`L Flexed Bicep Mean`,
           r_thigh=`R Mid Thigh Mean`,
           l_thigh=`L Mid Thigh Mean`,
           l_calf=`L Medial Calf Mean`)  %>% 
    rename_all(~ gsub(" Mean", "", .))  %>%
    distinct() %>% 
    slice_head(n=10) %>% 
    mutate_if(is.numeric, ~ round(., 2)) %>% 
    mutate(reporter= ifelse(is.na(reporter)," ",reporter))
  
  
  # if(!dir.exists(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","CSIP","anthro","Documents - Anthropometry","reports",AthleteData$Date[1]))){
  #   dir.create(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","CSIP","anthro","Documents - Anthropometry","reports",AthleteData$Date[1]))
  # }
  outfile_dir  <- here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","CSIP","anthro","Documents - Anthropometry","reports", AthleteData$Date[1])
  outfile_name <- paste0(AthleteData$AthleteName[1], "_", AthleteData$Date[1], ".pdf")
  
  if (!dir.exists(outfile_dir)) dir.create(outfile_dir, recursive = TRUE)
  
  quarto::quarto_render(
    input       = here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","CSIP","anthro","code","anthro_report_mct_param.qmd"),
    execute_params = list(AthleteData = AthleteData),
    output_format = "pdf",
    output_file  = outfile_name
  )
  
  # file.copy(filelist,
  #           here("~","CSI Pacific Dropbox","Ming-Chang Tsai","Tsai","CSIP","projects","Athletics","AC_para","IMU_workout","archive_data"),
  #           recursive = T)
  
  # file.remove(filelist)
}



