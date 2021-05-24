##
## scrape_players - get players from squad pages
## scrape_birthplace - get players bithplaces
## scrape_clubs - club team location and leagues
## scrape_teams - get national team colours and locations
## scrape_leagues - get league coefficients
##

library(tidyverse)
library(rvest)
library(janitor)

d <- tibble(
  year = seq(1960, 2020, 4),
  teams = c(rep(4, 5), rep(8, 4), rep(16, 5), rep(24, 2)),
  url = paste0("https://en.wikipedia.org/wiki/UEFA_Euro_", year, "_squads")
) %>%
  # wait for euro2020 squads
  slice(-n())

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
    unnest(cols = player)
  
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

d1 <- d0 %>%
  unnest(cols = c("players")) %>%
  clean_names() %>%
  select(-url, -teams) %>%
  mutate(pos = str_sub(string = pos, start = 2),
         captain = str_detect(string = player, pattern = "captain|Captain"),
         player_original = player,
         player = str_remove(string = player, pattern = " \\s*\\([^\\)]+\\)"))

# for(i in 1:nrow(d))
#   get_players(u = d$url[i], n = d$teams[i])
write_csv(d1 , "./data/players.csv")
