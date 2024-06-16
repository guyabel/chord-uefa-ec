##
## scrape_players: players in squads
## scrape_flag: national team flags
## scrape_comp: competition teams and logos
## scrape_colours: national team kits
##

library(tidyvese)
library(rvest)
library(countrycode)
library(janitor)
library(magick)

d <- tibble(
  year = seq(1960, 2024, 4),
  teams = c(rep(4, 5), rep(8, 4), rep(16, 5), rep(24, 3)),
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
  # u = e$url[1]
  # message(u)
  h <- read_html(u)
  
  xx <- h %>%
    html_nodes("table.sortable") %>%
    html_table(fill = TRUE) %>%
    tibble() %>%
    rename(tab = 1) %>%
    mutate(cn = map_chr(.x = tab, .f = ~paste0(names(.), collapse = "|"))) %>%
    mutate(k = 1:n()) %>%
    filter(str_detect(string = cn, pattern = "Qualified"))
  
  x <- xx %>%
    pull(tab) %>%
    .[[1]]
  
  x$url_team <- h %>%
    html_nodes("table.sortable") %>%
    .[[xx$k[1]]] %>%
    html_nodes("span a:nth-child(2)") %>%
    html_attr("href")
  
  return(x)
}

d <- d %>%
  mutate(url_comp_logo = map_chr(.x = url, .f = ~get_logo(u = .x)),
         team = map(.x = url, .f = ~get_teams(u = .x)))

d0 <- d %>%
  unnest(team) %>%
  clean_names() %>%
  mutate(team = ifelse(!is.na(team), team, team_a),
         team = str_remove(string = team, pattern = " \\s*\\([^\\)]+\\)"),
         team = str_remove_all(string = team, pattern = "\\[.*?\\]"),
         comp_prev = ifelse(!is.na(previous_appearances_in_tournament_a),
                            previous_appearances_in_tournament_a, 
                            previous_appearances_in_tournament_b)) %>%
  select(-team_a, -contains("previous_appearances_in_tournament"))

m <- c("CIS" = "CIS", 
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
                              destination = "iso3c", custom_match = m))
# d0 <- read_csv("./data/wiki_comp.csv")
write_excel_csv(d0,  "./data/wiki_comp.csv")

##
## download logos
##
d1 <- d0 %>%
  distinct(year, url_comp_logo)

for(i in 1:nrow(d1))
  d1$url_comp_logo[i] %>%
    paste0("https:", .) %>%
    image_read(density = 300) %>% 
    image_write(paste0("./logo/", d1$year[i], ".png"))
