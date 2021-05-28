##
## scrape_players: players in squads
## scrape_club: club information of players
## scrape_comp: euro competition teams and logos
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
) #%>%
  # wait for euro2020 squads
  # slice(-n())

get_players <- function(u, n){
  # u = d$url[i]
  # n = d$teams[i]
  h <- read_html(u)
  
  hh <- h %>%
    html_nodes("table.sortable")
  
  p <- hh %>%
    html_table() %>%
    .[1:n]
  
  s <- h %>%
    html_nodes(".mw-headline") %>%
    html_text() %>%
    str_subset(pattern = "Group", negate = TRUE) %>%
    .[1:n]
  
  p0 <- tibble(
    squad = s,
    player = p) %>%
    mutate(player = map(.x = player, .f = ~mutate(.x, across(everything(), as.character)))) %>%
    unnest(cols = player) %>%
    filter(Player != "")
  
  p0$url_player <- hh %>%
    .[1:n] %>%
    html_nodes(".nat-fs-player th a") %>%
    html_attr("href") %>%
    str_subset(pattern = "Captain", negate = TRUE) %>%
    # frank de boer in euro 1996
    {if(str_detect(string = u, pattern = "1996")) .[-28] else . } 
  
  
  c0 <- tibble(
    url = hh %>%
      .[1:n] %>%
      # html_nodes("td:nth-last-child(1") %>%
      html_nodes("td:nth-last-child(1) a") %>%
      html_attr("href"),
    
    text = hh %>%
      .[1:n] %>%
      html_nodes("td:nth-last-child(1) a") %>%
      html_text(),
    title = hh %>%
      .[1:n] %>%
      html_nodes("td:nth-last-child(1) a") %>%
      html_attr("title")
  ) %>%
    filter(!str_detect(string = url, pattern = "#cite")) %>%
    mutate(id = 1:n(), 
           id = ifelse(text == "", lead(id), id),
           id = as.character(id),
           id = fct_inorder(id),
           id = as.numeric(id))
  
  c1 <- c0 %>%
    filter(text != "") %>%
    rename(url_club = url,
           club = text, 
           club_title = title)
  
  c2 <- c0 %>%
    filter(text == "") %>%
    select(-text) %>%
    rename(url_club_country = url,
           club_country_title = title)
  
  c3 <- tibble(
    url_club_country = hh %>%
      .[1:n] %>%
      html_nodes(".flagicon") %>%
      html_nodes("a") %>%
      html_attr("href"),
    club_country = hh %>%
      .[1:n] %>%
      html_nodes(".flagicon") %>%
      html_nodes(".thumbborder") %>%
      html_attr("alt")
  ) %>%
    distinct()
  
  c4 <- c1 %>%
    left_join(c2, by = "id") %>%
    left_join(c3, by = "url_club_country")
  
  p1 <- p0 %>%
    mutate(id = 1:n()) %>%
    left_join(c4, by = "id")
  return(p1)
}

d0 <- d %>%
  mutate(players = map2(.x = url, .y = teams, 
                        .f = ~get_players(u = .x, n = .y)))

# for(i in 1:nrow(d))
#   get_players(u = d$url[i], n = d$teams[i])

d1 <- d0 %>%
  unnest(cols = c("players")) %>%
  clean_names() %>%
  select(-url, -teams) %>%
  mutate(pos = str_sub(string = pos, start = 2),
         captain = str_detect(string = player, pattern = "captain|Captain"),
         player_original = player,
         player = str_remove(string = player, pattern = " \\s*\\([^\\)]+\\)"))

# d1 <- read_csv("./data/wfnet_players.csv")
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
    club_country2 = case_when(
      club_country == "Soviet Union" ~ "USSR",
      club_country == "Socialist Federal Republic of Yugoslavia" ~ "Yugoslavia",
      club_country == "Federal Republic of Yugoslavia"  ~ "FR Yugoslavia",
      TRUE ~ club_country),
    nat_team_alpha3 = countrycode(
      sourcevar = nat_team, origin = "country.name", 
      destination = "iso3c", custom_match = cm),
    club_alpha3 = countrycode(
      sourcevar = club_country2, origin = "country.name", 
      destination = "iso3c", custom_match = cm),
  )

n0 <- d2 %>%
  select(nat_team, nat_team_alpha3) %>%
  distinct() %>%
  rename(label = nat_team, 
         alpha3 = nat_team_alpha3)

c0 <- d2 %>%
  select(club_country2, club_alpha3) %>%
  distinct() %>%
  rename(label = club_country2, 
         alpha3 = club_alpha3)

n0 %>%
  bind_rows(c0) %>%
  distinct() %>%
  group_by(alpha3) %>%
  mutate(n = n()) %>%
  arrange(desc(n))

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
