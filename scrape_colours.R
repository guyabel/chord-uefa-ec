##
## scrape_players: players in squads
## scrape_flag: national team flags
## scrape_comp: competition teams and logos
## scrape_colours: national team kits
##

library(tidyverse)
library(rvest)
library(countrycode)

d <- read_csv("./data/wiki_comp.csv") %>%
  select(team_alpha3, url_team) %>%
  distinct()

get_kit_colours <- function(u){
  # u = d$url_team[14]
  h <- paste0("https://en.wikipedia.org/", u) %>%
    read_html()
  
  #kit colour
  tibble(
    colours = h %>%
      html_nodes(".infobox table td:nth-child(1) div:nth-child(1)") %>%
      # html_nodes(".infobox table td:nth-child(1) div:nth-child(1)  div:nth-child(1)") %>%
      # html_attr("style") %>%
      as.character() %>%
      paste0(collapse = ";") %>%
      str_split(";|[>]") %>% 
      .[[1]] %>%
      str_subset("background-color") %>%
      str_remove("background-color:") %>%
      str_trim() %>%
      str_subset(".{7,}") %>%
      str_sub(end = 7) %>%
      unique()
  )
}

d0 <- d %>%
  mutate(kit = map(.x = url_team, .f = ~get_kit_colours(u = .x)))


d1 <- 
  d0 %>%
  unnest(kit) %>%
  group_by(url_team) %>%
  mutate(n = 1:n()) %>%
  pivot_wider(names_from = n, values_from = colours, values_fill = NA, 
              names_prefix = "c")

write_excel_csv(x = d1, file = "./data/wiki_colours.csv")
# d0 <- read_csv("./data/wiki_colours.csv")
# pie(rep(1, nrow(d0)), labels = d0$team_alpha3, col = d0$kit_shirt)

# h <- read_html("https://en.wikipedia.org/wiki/National_colours")
# 
# d <- tibble(
#   country = h %>% 
#     # html_table(fill = TRUE)
#     html_nodes("table") %>%
#     .[1:14] %>%
#     html_nodes("td:nth-child(1)") %>%
#     html_text(trim = TRUE),
#   colour1_name = h %>% 
#     html_nodes("table") %>%
#     .[1:14] %>%
#     html_nodes("td:nth-child(3)") %>%
#     html_text(trim = TRUE),
#   colour2_name = h %>% 
#     html_nodes("table") %>%
#     .[1:14] %>%
#     html_nodes("td:nth-child(4)") %>%
#     html_text(trim = TRUE)
# )
# c1 <- h %>% 
#     html_nodes("table") %>%
#     .[1:14] %>%
#     html_nodes("td:nth-child(5)")
# c2 <- h %>% 
#     html_nodes("table") %>%
#     .[1:14] %>%
#     html_nodes("td:nth-child(6)") 
# 
# get_colour_code <- function(x){
#   x %>%
#     html_nodes(".legend-color") %>%
#     as.character() %>%
#     str_extract_all(pattern = "background-color:\\s*(.*?)\\s*;") %>%
#     unlist() %>%
#     str_remove(pattern = "background-color:") %>%
#     str_remove(pattern = ";") %>%
#     paste(collapse = ",")
# }
# 
# 
# d$colour1_code <- NA
# d$colour2_code <- NA
# for(i in 1:nrow(d)){
#   d$colour1_code[i] <- get_colour_code(x = c1[i])
#   d$colour2_code[i] <- get_colour_code(x = c2[i])
# }
# 
# cm <- c("CIS" = "CIS", 
#         "CSSR" = "CSK",
#         "England" = "GB-ENG", 
#         "Northern Ireland" = "GB-NIR",
#         "Scotland" = "GB-SCT",
#         "Wales" = "GB-WLS", 
#         "Yugoslavia" = "YUG", 
#         "Soviet Union" = "SUN",
#         "USSR" = "SUN")
# d <- d %>%
#   mutate(alpha3 = countrycode(sourcevar = country, 
#                               origin = "country.name", 
#                               destination = "iso3c", 
#                               custom_match = cm))
# 
# write_excel_csv(x = d, file = "./data/wiki_colours.csv")
# 
# dd <- read_csv(file = "./data/wfnet_players.csv") %>%
#   select(year, team, alpha3) %>%
#   distinct() %>%
#   left_join(d) %>%
#   arrange(team) %>%
#   select(-contains("name")) %>%
#   separate(col = colour1_code, into = c("col1", "col2"), 
#            remove = FALSE, extra = "drop", fill = "right") %>%
#   filter(!str_detect(string = country, pattern = "[0-9]"))
# 
# write_excel_csv(x = dd, file = "./data/wiki_colours2.csv")
# # manually edit in colours_team.xlxs the team colours