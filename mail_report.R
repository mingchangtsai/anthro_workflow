suppressWarnings(library(blastula))
suppressWarnings(library(glue))

# https://github.com/rstudio/blastula
# https://thecoatlessprofessor.com/programming/r/sending-an-email-from-r-with-blastula-to-groups-of-students/

# Run once to generate credentials GMAIL
# create_smtp_creds_key(
#   id = "Gmail",          # Name the Credential
#   user = "tsai.ming.chang@gmail.com",
#   provider = "gmail",       # Provider
#   use_ssl = TRUE,                # Ensure SSL is used
#   # password: ujww wgkn zdvt twnm  # use this password for 3rd party sending email
#   overwrite=TRUE
# )

### credential for Microsoft 365
# create_smtp_creds_key(
#   id = "CSIP_outlook",          # Name the Credential
#   user = "mtsai@csipacific.ca",  # User E-mail Address
#   provider = "office365",       # Provider
#   # host = "smtp.office365.com",
#   # port = 587,
#   use_ssl = TRUE,                # Ensure SSL is used
#   overwrite=TRUE
#   # password: Vaz55195
# )



# sender <- "traininggroundrbc@canadianolympics.onmicrosoft.com" # sender email address
# sender <- "traininggroundrbc@olympic.ca" # sender email address
sender <- "tsai.ming.chang@gmail.com"
# sender <- "rbctrainingground2023@gmail.com"

group_email_template = function() {
  
  # Customize e-mail title
  title = mtcars %>% 
    glue_data("Anthro progress status")
  
  # Construct the e-mail for the Team.
  email = mtcars %>% 
    glue_data(
      {paste0("Anthro data uploaded to Smartabase and the report(s) can be found in the sharepoint folder",
              "<br />",
              "~/Documents - Anthropometry/reports",
              "<br />",
              "https://csip.sharepoint.com/:f:/s/Anthropometry/EigqsIsXn81MtzNj0pAZZKQBAgTyhHofCyfJZVvHGP0ZSg?e=JePTqf",
              "<br />","<br />",
              "Data Solutions"
      )}
    ) %>%
    md() %>%
    compose_email(title = title)
  
  out = list("email" = email, "title" = title)
  
  return(out)
}


# Retrieve current team
# group_data = email_group_df[i, ] 
# group_data <- testdata[b,]

# Construct the e-mail using our custom template.
# Returns a list with `email` and `title`
email_contents <- group_email_template()

# Generate a to-field
# to <- mydata$email[m]
# to <- c("ming_chang_tsai@hotmail.com","jsorenson@olympic.ca")

# options (timeout =30000)
# Send e-mail
email_contents$email %>%
  # add_attachment(file="report_card_EN.html") %>%
  # add_attachment(file="report_card_FR.html") %>%
  smtp_send(
    from = sender,
    to = to,
    subject = email_contents$title,
    # credentials = creds(
    #   user = "traininggroundrbc@canadianolympics.onmicrosoft.com",
    #   provider = "office365",
    # )
    # credentials = creds_key(id="RBCTG_outlook")
    # credentials = creds_key(id = "Gmail")
    credentials = creds_file(here("~","OneDrive - Canadian Sport Institute Pacific","Tsai","CSIP","projects","CSIP","anthro","code","Gmail"))
    
  )


