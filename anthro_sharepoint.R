
# https://github.com/Azure/Microsoft365R
# https://www.r-bloggers.com/2021/02/microsoft365r-an-r-interface-to-the-microsoft-365-suite/
### sharepoint api
# library(Microsoft365R)
library(pacman)
p_load(tidyverse,conflicted,Microsoft365R)
conflict_prefer("filter","dplyr")
options(microsoft365r_use_cli_app_id=TRUE)


# AzureAuth::clean_token_directory()
# AzureGraph::delete_graph_login()

# get_team("My team",tenant="9798e3e4-0f1a-4f96-91ad-b31a4229413a",app="30fe8fb3-a061-47e2-afbd-2b5a99a73833")

# get_sharepoint_site("http://localhost:1410",
#                     tenant="9798e3e4-0f1a-4f96-91ad-b31a4229413a",
#                     app="30fe8fb3-a061-47e2-afbd-2b5a99a73833",
#                     )

site <- get_sharepoint_site(site_url="https://csip.sharepoint.com/sites/Anthropometry",
                        tenant="9798e3e4-0f1a-4f96-91ad-b31a4229413a",
                        app="30fe8fb3-a061-47e2-afbd-2b5a99a73833")

lst <- site$get_list("master_database")

