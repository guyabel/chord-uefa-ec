##
## scrape_players: players in squads
## scrape_flag: national team flags
## scrape_colours: national team kits
## scrape_comp: competition teams and logos
##

library(tidyverse)
library(rvest)
library(countrycode)
library(janitor)

##
## wikipedia
##

d <- tibble(
  year = seq(1960, 2020, 4),
  teams = c(rep(4, 5), rep(8, 4), rep(16, 5), rep(24, 2)),
  url = paste0("https://en.wikipedia.org/wiki/UEFA_Euro_", year, "_squads")
) 


get_players <- function(u, n){
  # u = d$url[i];u
  # n = d$teams[i]
  h <- read_html(u)
  
  hh <- h %>%
    html_nodes("table.sortable")
  
  p <- tibble(
    squads = hh %>%
      html_table()
  ) %>%
    mutate(nn = map(.x = squads, .f = ~nrow(x = .x)), 
           nn = unlist(nn),
           squad_list = nn >= 11, 
           table_no = cumsum(squad_list), 
           squad_list = ifelse(table_no > n, FALSE, squad_list))
  
  
  s <- h %>%
    html_nodes(".mw-headline") %>%
    html_text() %>%
    str_subset(pattern = "Group", negate = TRUE) %>%
    .[1:n]
  
  not_line <- hh %>%
    .[p$squad_list] %>%
    html_nodes("td:nth-last-child(1)") %>%
    html_attr("colspan") %>%
    is.na()
  
  tibble(
    squad = s,
    player = p$squads[p$squad_list]
  ) %>%
    mutate(
      player = map(
        .x = player, 
        .f = ~mutate(.x, across(everything(), as.character))
      )) %>%
    unnest(cols = player) %>%
    filter(Player != "") %>%
    mutate(player_url = hh %>%
             .[p$squad_list] %>%
             html_nodes(".nat-fs-player th") %>%
             html_node("a") %>%
             html_attr("href"),
           club_fa_url = hh %>%
             .[p$squad_list] %>%
             html_nodes("td:nth-last-child(1)") %>%
             .[not_line] %>%
             html_node("a") %>%
             html_attr("href"),
           club_fa = hh %>%
             .[p$squad_list] %>%
             html_nodes("td:nth-last-child(1)") %>%
             .[not_line] %>%
             html_node("a") %>%
             html_attr("title"),
           club = hh %>%
             .[p$squad_list] %>%
             html_nodes("td:nth-last-child(1)") %>%
             .[not_line] %>%
             html_text(trim = TRUE),
           club_country = hh %>%
             .[p$squad_list] %>%
             html_nodes("td:nth-last-child(1)") %>%
             .[not_line] %>%
             html_node(".flagicon") %>%
             html_node(".thumbborder") %>%
             html_attr("alt"),
           club_country_flag = hh %>%
             .[p$squad_list] %>%
             html_nodes("td:nth-last-child(1)") %>%
             .[not_line] %>%
             html_node(".flagicon") %>%
             html_node("img") %>%
             html_attr("src")
    )
}

d0 <- d %>%
  mutate(players = map2(.x = url, .y = teams, 
                        .f = ~get_players(u = .x, n = .y)))

# i = 16
# get_players(u = d$url[i], n = d$teams[i]) %>%
#   group_by(squad) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n)) %>%
#   print(n = 30)
# for(i in 1:nrow(d))
#   get_players(u = d$url[i], n = d$teams[i])

d1 <- d0 %>%
  unnest(cols = c("players")) %>%
  clean_names() %>%
  select(-url, -teams) %>%
  mutate(pos = str_sub(string = pos, start = 2),
         captain = str_detect(string = player, pattern = "captain|Captain|\\(c\\)"),
         player_original = player,
         player = str_remove(string = player, pattern = " \\s*\\([^\\)]+\\)"))

cm <- c("CIS" = "CIS", 
        "CSSR" = "CSK",
        "Czechoslovakia" = "CSK",
        "England" = "GB-ENG", 
        "Northern Ireland" = "GB-NIR",
        "Scotland" = "GB-SCT",
        "Wales" = "GB-WLS", 
        "FR Yugoslavia" = "SCG", 
        "Yugoslavia" = "YUG", 
        "Soviet Union" = "SUN",
        "USSR" = "SUN")

d2 <- d1 %>%
  mutate(
    nat_team = case_when(
      squad == "Soviet Union" ~ "USSR",
      TRUE ~ squad
    ),
    club_country_harm = case_when(
      club_country == "Wales" ~ "England",
      club_country == "Soviet Union" ~ "USSR",
      club_country == "Socialist Federal Republic of Yugoslavia" ~ "Yugoslavia",
      club_country == "Federal Republic of Yugoslavia"  ~ "FR Yugoslavia",
      TRUE ~ club_country),
    nat_team_alpha3 = countrycode(
      sourcevar = nat_team, origin = "country.name", 
      destination = "iso3c", custom_match = cm),
    club_alpha3 = countrycode(
      sourcevar = club_country_harm, origin = "country.name", 
      destination = "iso3c", custom_match = cm),
    club_country_flag = str_remove(string = club_country_flag, pattern = "thumb"),
    club_country_flag = str_remove(string = club_country_flag, pattern = "/[^/]+$")
  )

# checks on nat_team and club_country_harm match

d2 %>%
  # filter(is.na(nat_team))
  # filter(is.na(nat_team_alpha3))
  # filter(is.na(club_country_harm))
  filter(is.na(club_alpha3))

x = d2 %>%
  select(nat_team, club_country_harm) %>%
  pivot_longer(cols = 1:2, names_to = "type", values_to = "label") %>%
  select(-type) %>%
  distinct() %>%
  arrange(label)
# group_by(label) %>%
# mutate(n = n()) %>%
# arrange(desc(n))

write_excel_csv(d2 , "./data/wiki_players.csv")


##
## worldfootball.net
##

# h <- read_html("https://www.worldfootball.net/history/em/")
# 
# d <- tibble(
#   year = h %>% 
#     html_nodes("td:nth-child(4) a") %>%
#     html_attr("title") %>%
#     str_extract("\\d{4}"),
#   url_euro_squads = h %>% 
#     html_nodes("td:nth-child(4) a") %>%
#     html_attr("href") %>%
#     paste0("https://www.worldfootball.net", .)
# ) %>%
#   # waiting for EURO2020 squads
#   slice(-1)
# 
# get_squad_links <- function(u){
#   # h <- read_html(d$url_squads[i])
#   h <- read_html(u)
#   
#   s <- tibble(
#     team = h %>%
#       html_nodes("td:nth-child(2) a") %>%
#       html_text(),
#     url_team = h %>%
#       html_nodes("td:nth-child(2) a") %>%
#       html_attr("href") %>%
#       paste0("https://www.worldfootball.net", .),
#     team_img = h %>%
#       html_nodes(".standard_tabelle td:nth-child(1) img") %>%
#       html_attr("src") %>%
#       paste0("https://www.worldfootball.net", .) %>%
#       str_replace(pattern = "mini", replacement = "mittel"),
#     url_squad = h %>%
#       html_nodes("td:nth-child(6) a") %>%
#       html_attr("href") %>%
#       paste0("https://www.worldfootball.net", .)
#   )
#   return(s)
# }
# 
# d0 <- d %>%
#   mutate(teams = map(.x = url_euro_squads, .f = ~get_squad_links(u = .x))) %>%
#   unnest(teams)
# 
# 
# get_squad <- function(u){
#   # h <- read_html(d0$url_squad[156])
#   # h <- read_html(d0$url_squad[1])
#   
#   h <- read_html(u)
#   x1 <- h %>%
#     # html_nodes(".portfolio .box") %>%
#     html_table(fill = TRUE, header = FALSE) %>%
#     .[[2]] %>%
#     as_tibble() %>%
#     mutate(X1 = ifelse(X1 == "", NA, X1)) %>%
#     fill(X1, .direction = "down") %>%
#     filter(X4 == "") %>%
#     select(-X4, -X7) %>%
#     set_names(c("position", "squad_n", "player", "club", "dob"))
#   
#   x2 <- tibble(
#     url_player = h %>%
#       html_nodes("td:nth-child(3) a") %>%
#       html_attr("href") %>%
#       paste0("https://www.worldfootball.net", .),
#     player = h %>%
#       html_nodes("td:nth-child(3) a") %>%
#       html_attr("title")
#   )
#   
#   x3 <- tibble(
#     url_club = h %>%
#       html_nodes("td:nth-child(5) a") %>%
#       html_attr("href") %>%
#       paste0("https://www.worldfootball.net", .),
#     club = h %>%
#       html_nodes("td:nth-child(5) a") %>%
#       html_text()
#   ) %>%
#     distinct()
#   
#   x <- x1 %>%
#     left_join(x2, by = "player") %>%
#     left_join(x3, by = "club")
# }
# 
# d1 <- d0 %>%
#   mutate(squad = map(.x = url_squad, .f = ~get_squad(u = .x))) %>%
#   unnest(squad) %>%
#   mutate(staff = str_detect(string = position, pattern = "Manager|Coach"),
#          staff = ifelse(year == 2012, player == "Marcus Allbäck", TRUE, staff))
# 
# # d1 <- read_csv("./data/wfnet_players.csv")
# cm <- c("CIS" = "CIS", 
#         "CSSR" = "CSK",
#         "Czechoslovakia" = "CSK",
#         "England" = "GB-ENG", 
#         "Northern Ireland" = "GB-NIR",
#         "Scotland" = "GB-SCT",
#         "Wales" = "GB-WLS", 
#         "FR Yugoslavia" = "SCG", 
#         "Yugoslavia" = "YUG", 
#         "Soviet Union" = "SUN",
#         "USSR" = "SUN")
# 
# d2 <- d1 %>%
#   mutate(
#     team = ifelse(team == "Germany" & year < 1992, "West Germany", team),
#     team = ifelse(team == "CSSR", "Czechoslovakia", team),
#     team = ifelse(team == "Yugoslavia" & year == 2000, "FR Yugoslavia", team),
#     team_alpha3 = countrycode(
#       sourcevar = team, origin = "country.name", 
#       destination = "iso3c", custom_match = cm)
#   )
# 
# write_excel_csv(d2, "./data/wfnet_players.csv")
# 
