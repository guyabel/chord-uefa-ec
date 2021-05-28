##
## scrape_players: players in squads
## scrape_club: club information of players
## scrape_comp: euro competition teams and logos
##

library(tidyvese)
library(rvest)
library(countrycode)
library(janitor)

d <- tibble(
  year = seq(1960, 2020, 4),
  teams = c(rep(4, 5), rep(8, 4), rep(16, 5), rep(24, 2)),
  url = paste0("https://en.wikipedia.org/wiki/UEFA_Euro_", year)
)

get_logo <- function(u){
  # u = d$url[1]
  h <- read_html(u)
  h %>%
    html_nodes(".infobox-image img") %>%
    html_attr("src")
}

get_teams <- function(u){
  # u = d$url[i]
  h <- read_html(u)
  
  k <- 1
  if(str_detect(string = u, pattern = "2020"))
    k <- 2
  d <- h %>%
    html_nodes("table.sortable") %>%
    .[[k]] %>%
    html_table(fill = TRUE)
  
  d$url_team <- h %>%
    html_nodes("table.sortable") %>%
    .[[k]] %>%
    html_nodes("span a:nth-child(2)") %>%
    html_attr("href")
  
  return(d)
}

d <- d %>%
  mutate(url_comp_logo = map(.x = url, .f = ~get_logo(u = .x)),
         team = map(.x = url, .f = ~get_teams(u = .x)))

d0 <- d %>%
  unnest(url_comp_logo) %>%
  unnest(team) %>%
  clean_names() %>%
  mutate(team = ifelse(!is.na(team), team, team_a),
         team = str_remove(string = team, pattern = " \\s*\\([^\\)]+\\)"),
         team = str_remove_all(string = team, pattern = "\\[.*?\\]"),
         comp_prev = ifelse(!is.na(previous_appearances_in_tournament_a),
                            previous_appearances_in_tournament_a, 
                            previous_appearances_in_tournament_b)) %>%
  select(-team_a, -contains("previous_appearances_in_tournament"))

cm <- c("CIS" = "CIS", 
        "CSSR" = "CZK",
        "Czechoslovakia" = "CSK",
        "England" = "GB-ENG", 
        "Northern Ireland" = "GB-NIR",
        "Scotland" = "GB-SCT",
        "Wales" = "GB-WLS", 
        "Yugoslavia" = "YUG", 
        "FR Yugoslavia" = "SCG",
        "Soviet Union" = "SUN",
        "USSR" = "SUN")

d0 <- d0 %>%
  mutate(team_alpha3 = countrycode(sourcevar = team, origin = "country.name", 
                              destination = "iso3c", custom_match = cm))
# d0 <- read_csv("./data/wiki_comp.csv")
write_excel_csv(d0,  "./data/wiki_comp.csv")