library(tidyverse)
library(countrycode)

d0 <- read_csv("./data/wfnet_club.csv")
d1 <- read_csv("./data/wfnet_players.csv")

d1 %>%
  drop_na(url_club) %>%
  select(url_club, year) %>%
  distinct()

# league of club at time of tournament.. not possible everywhere
get_club_matches <- function(u, y){
  # u <- d2$url_club[i]; y = d2$year[i]
  # h <- read_html(d0$url_squad[1])

  h <- paste0(u, "/", y,"/3") %>%
    read_html()

  x0 <- h %>%
    # html_nodes(".portfolio .box") %>%
    html_table(fill = TRUE, header = FALSE) %>%
    .[[2]] %>%
    as_tibble()

  if(ncol(x0) == 1)
    x1 <- NA

  if(ncol(x0) > 1){
    x1 <- x0 %>%
      mutate(X8 = ifelse(X8 == "", NA, X8)) %>%
      fill(X8, .direction = "down") %>%
      filter(str_length(X4) == 1) %>%
      group_by(X8) %>%
      count() %>%
      ungroup() %>%
      rename(club_comp = 1,
             matches = n)
  }
  return(x1)
}


d2 <- d1 %>%
  drop_na(url_club) %>%
  select(year, url_club) %>%
  distinct() %>%
  mutate(m = map2(.x = url_club, .y = year,
                  .f = ~get_club_matches(u = .x, y = .y))) %>%
  unnest(m)

d3 <- d2 %>%
  group_by(url_club, year) %>%
  arrange(desc(matches)) %>%
  slice(1) %>%
  mutate(
    club_comp = ifelse(
      str_detect(string = club_comp, pattern = "Europa"),
      NA, club_comp),
    club_comp = ifelse(
      str_detect(string = club_comp, pattern = "Champions League "),
      NA, club_comp),
    club_comp = ifelse(
      str_detect(string = club_comp, pattern = "Cup Winners Cup "),
      NA, club_comp))


d4 <- d0 %>%
  select(url_club, country) %>%
  distinct() %>%
  right_join(d3)


# d4 <- read_csv("./data/wfnet_leagues.csv")
cm <- c("CIS" = "CIS", 
        "CSSR" = "CZK",
        "Czechoslovakia" = "CSK",
        "England" = "GB-ENG", 
        "Northern Ireland" = "GB-NIR",
        "Scotland" = "GB-SCT",
        "Wales" = "GB-WLS", 
        "West Germany" = "FRG",
        "Yugoslavia" = "YUG", 
        "FR Yugoslavia" = "SCG",
        "Soviet Union" = "SUN",
        "USSR" = "SUN")

# d4 <- read_csv("./data/wfnet_leagues.csv")
d5 <- d4 %>%
  mutate(
    league = case_when(
      country == "Germany" & year < 1992 ~ "West Germany",
      country == "Czech Republic" & year < 1994 ~ "Czechoslovakia",
      country == "Slovakia" & year < 1994 ~ "Czechoslovakia",
      country == "Wales" ~ "England",
      str_detect(string = club_comp, pattern = "Prva Liga") ~ "Yugoslavia",
      country == "Serbia" & year == 2000 ~ "FR Yugoslavia",
      str_detect(string = club_comp, pattern = "Vysshaya Liga") ~ "USSR",
      TRUE ~ country),
    league_alpha3 = countrycode(
      sourcevar = league, origin = "country.name", 
      destination = "iso3c", custom_match = cm)
  )
write_excel_csv(d5, "./data/wfnet_leagues.csv")
