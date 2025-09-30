library(pacman)
p_load(tidyverse,smartabaseR,conflicted)
conflict_prefer("filter","dplyr")

## https://teamworksapp.github.io/smartabaseR/index.html


mydata <- sb_get_event(
  form = "ISAK Anthros",
  date_range = sb_date_range("7", "years"),
  url = "https://canadiansport.smartabase.com/csip",
  username = "ming_chang_tsai",
  password = "MctRow!ng042422",
  # user_key = "username",
  # user_value = "Mika Kimmins"
  user_key = "user_guid",
  user_value = "39999"
  # filter = sb_get_event_filter(
  #   data_key = "event_id",
  #   data_value = "2699994",
  #   data_condition = "equal_to"
  # ),
  # option = sb_get_event_option(
  #   # cache = T,
  #   interactive_mode = FALSE
  # )
)


