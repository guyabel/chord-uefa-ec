library(tidyverse)
library(countrycode)
library(magick)

w1 <- read_csv("./data/wiki_players.csv")
# x1 <- read_csv("./data/wfnet_players.csv")
# x2 <- read_csv("./data/wfnet_leagues.csv")

n0 <- w1 %>%
  select(nat_team, nat_team_alpha3) %>%
  distinct() %>%
  rename(label = nat_team, 
         alpha3 = nat_team_alpha3)

c0 <- w1 %>%
  select(club_country2, club_alpha3) %>%
  distinct() %>%
  rename(label = club_country2, 
         alpha3 = club_alpha3) %>%
  drop_na()

cm <- c("CIS" = "CIS", 
        "Czechoslovakia" = "CZ",
        "England" = "GB-ENG", 
        "Northern Ireland" = "GB-NIR",
        "Scotland" = "GB-SCT",
        "Wales" = "GB-WLS", 
        "West Germany" = "DE",
        "Yugoslavia" = "YU", 
        "FR Yugoslavia" = "SCG",
        "Soviet Union" = "SU",
        "USSR" = "SU", 
        "No Club" = "")

d0 <- n0 %>%
  bind_rows(c0) %>%
  distinct() %>%
  mutate(
    alpha2 = countrycode(
      sourcevar = label, origin = "country.name", 
      destination = "iso2c", custom_match = cm),
    alpha2 = str_to_lower(string = alpha2),
    flag_url = case_when(
      # alpha2 %in% c("gb-eng", "gb-wls", "gb-nir") ~ paste0("/regions_flags/gb/", alpha2),
      alpha2 %in% c("su", "yu") ~ paste0("https://raw.githubusercontent.com/kent1D/svg-flags/master/historical_flags/", alpha2, ".svg"),
      # alpha2 == "gb-sct" ~ "https://raw.githubusercontent.com/lipis/flag-icon-css/master/flags/4x3/gb-sct.svg",
      alpha2 == "scg" ~ "https://upload.wikimedia.org/wikipedia/commons/7/7e/Flag_of_Yugoslavia_%281992%E2%80%932003%29%3B_Flag_of_Serbia_and_Montenegro_%282003%E2%80%932006%29.svg",
      alpha2 == "cis" ~ "https://raw.githubusercontent.com/kent1D/svg-flags/master/io_flags/cis.svg",
      TRUE ~ paste0("", alpha2)
    ), 
    flag_url = ifelse(
      alpha2 %in% c("scg", "cis", "su", "yu"), flag_url,
      # paste0("https://raw.githubusercontent.com/kent1D/svg-flags/master/", flag_url, ".svg")
      paste0("https://raw.githubusercontent.com/lipis/flag-icon-css/master/flags/4x3/", flag_url, ".svg")
    )
)

for(i in 1:nrow(d0)){
  message(d0$alpha3[i])
  f <- image_read_svg(path = d0$flag_url[i])
  g <- "center"
  if(d0$alpha3[i] == "SUN")
    g <- "west"
  f %>%
    image_scale("x75") %>%
    image_crop("100", gravity = g) %>%
    image_write(path = paste0("./flags/",d0$alpha3[i], ".png"))
    # image_write(path = paste0("./flags/",d0$alpha3[i], ".svg"), format = "svg")
  
  # image_read(paste0("./flags/",d0$alpha3[i], ".svg"))
  
}