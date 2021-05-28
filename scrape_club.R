library(tidyverse)
library(rvest)
library(janitor)

d1 <- read_csv("./data/wfnet_players.csv")

# no club
d1 %>%
  filter(is.na(url_club), !staff)

get_club_info <- function(u){
  # u <- d2$url_club[i]
  h <- paste0(u, "/1") %>%
    read_html()
  x1 <- x2 <- x3 <- NULL
  x1 <- h %>%
    # html_nodes(".portfolio .box") %>%
    html_table(fill = TRUE, header = FALSE) %>%
    .[[1]] %>%
    as_tibble() %>%
    set_names(c("m", "info"))
  
  x2 <- tibble(
    m = "url_venue",
    info = h %>%
      html_nodes(".portfolio a") %>%
      html_attr("href") %>%
      str_subset(pattern = "venue") %>%
      paste0("https://www.worldfootball.net", .)
  )
  
  x3 <- tibble(
    m = "url_club_img",
    info = h %>% 
      html_nodes(".emblem img") %>%
      html_attr("src")
  )
  bind_rows(x1, x2, x3)   
}

d2 <- d1 %>%
  select(club, url_club) %>%
  distinct() %>%
  filter(!is.na(url_club)) %>%
  mutate(i = map(.x = url_club, .f = ~get_club_info(u = .x))) %>%
  unnest(i) %>%
  pivot_wider(names_from = m, values_from = info) %>%
  clean_names()
write_excel_csv(d2, "./data/wfnet_club.csv")
