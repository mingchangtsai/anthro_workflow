library(pacman)
p_load(tidyverse, smartabaseR, conflicted)
conflict_prefer("filter","dplyr")

# pull creds from .Renviron if you set them
url  <- Sys.getenv("SMARTABASE_URL",  "https://canadiansport.smartabase.com/csip")
user <- Sys.getenv("SMARTABASE_USER", "your_login")
pass <- Sys.getenv("SMARTABASE_PASS", "your_password")

# 1) Start with NO filters to confirm access + form name
mydata <- sb_get_event(
  form       = "ISAK Anthros",            # exact name
  date_range = sb_date_range(7, "years"),
  url        = url,
  username   = user,
  password   = pass,
  option     = sb_get_event_option(interactive_mode = FALSE)
)

# 2) Then add a PERSON filter via sb_get_event_filter()
mydata_one_user <- sb_get_event(
  form       = "ISAK Anthros",
  date_range = sb_date_range(7, "years"),
  url        = url, username = user, password = pass,
  filter     = sb_get_event_filter(
    user_key   = "username",              # or "user_guid" if you have the real GUID string
    user_value = "api.user"
  ),
  option     = sb_get_event_option(interactive_mode = FALSE)
)

# 3) If you also want a DATA filter, include it in the same filter call:
mydata_user_event <- sb_get_event(
  form       = "ISAK Anthros",
  date_range = sb_date_range(7, "years"),
  url        = url, username = user, password = pass,
  filter     = sb_get_event_filter(
    user_key       = "username",
    user_value     = "exact_login_here",
    data_key       = "event_id",
    data_value     = "2699994",
    data_condition = "equal_to"           # e.g., equal_to / greater_than / less_than
  ),
  option     = sb_get_event_option(interactive_mode = FALSE)
)

# 4) Multiple filters? Pass a list of sb_get_event_filter() objects:
filters <- list(
  sb_get_event_filter(user_key="username", user_value="exact_login_here"),
  sb_get_event_filter(data_key="event_id", data_value="2699994", data_condition="equal_to")
)

mydata_multi <- sb_get_event(
  form       = "ISAK Anthros",
  date_range = sb_date_range(7, "years"),
  url        = url, username = user, password = pass,
  filter     = filters,
  option     = sb_get_event_option(interactive_mode = FALSE)
)
