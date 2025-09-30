library(pacman)
p_load(tidyverse,smartabaseR,conflicted)
conflict_prefer("filter","dplyr")

## https://teamworksapp.github.io/smartabaseR/index.html


sb_insert_event(
  df = testdata,
  form = "ISAK Anthros",
  url = "https://canadiansport.smartabase.com/csip",
  username = "ming_chang_tsai",
  password = "MctRow!ng042422"
  # option = sb_get_event_option(
  #   interactive_mode = FALSE
  # )
)
