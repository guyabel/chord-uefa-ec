##
## scrape_players - get players from squad pages
## scrape_birthplace - get players bithplaces
## scrape_clubs - club team location and leagues
## scrape_teams - get national team colours and locations
## scrape_leagues - get league coefficients
##

library(tidyverse)
library(rvest)
library(sp)

d <- read_csv("./data/players.csv")

##
## club info
##

d0 <- d %>%
  # select(contains("club"))
  select(url_club) %>%
  distinct() %>%
  mutate(club_league2021 = NA, 
         url_club_stadium = NA) %>%
  filter(!str_detect(string = url_club, pattern = "redlink"))

for(i in 1:nrow(d0)){
  h1 <- paste0("https://en.wikipedia.org", d0$url_club[i]) %>%
    read_html()
  
  v <- h1 %>%
    html_nodes(".vcard")
  if(length(v) == 0)
    next()
  
  #league
  l <- h1 %>%
    html_nodes(".vcard") %>%
    .[[1]] %>%
    html_table(fill = TRUE) %>% 
    subset(X1=="League") %>% 
    .[[2]]
  
  #ground wiki page
  s <- h1 %>%
    html_nodes(".vcard") %>%
    .[[1]] %>%
    html_nodes(".label a") %>%
    .[1] %>%
    html_attr("href")
  
  if(length(l) != 0)
    d0$club_league2021[i] <- l 
  if(length(s) != 0)
    d0$url_club_stadium[i] <- s
    
  if(i %% 10 == 0)
    message(i)
}

##
## club stadium geo
##

d1 <- d0 %>%
  filter(!is.na(url_club_stadium)) %>%
  filter(!str_detect(string = url_club_stadium, pattern = "redlink")) %>%
  mutate(club_stadium_coord = NA)

for(i in 1:nrow(d1)){
  #birth place
  h1 <- paste0("https://en.wikipedia.org/", d1$url_club_stadium[i]) %>%
    read_html()
  
  geo <- h1 %>%
    html_nodes(".geo-dms") %>%
    .[1] %>%
    html_text()
  
  if(length(geo) == 0) 
    next()
  d1$club_stadium_coord[i] <- geo
  if(i %% 10 == 0){
    message(i)
  }
}

d2 <- d1 %>%
  filter(!is.na(club_stadium_coord)) %>%
  rename(coord = club_stadium_coord) %>%
  separate(col = "coord", into = c("lat", "lng"), 
           sep = " ", remove = FALSE) %>%
  mutate(lat = str_extract_all(string = lat, pattern = "[0-9.]+"),
         lng = str_extract_all(string = lng, pattern = "[0-9.]+"),
         lat = map(.x = lat, .f = ~paste0(.x, collapse = "-")),
         lng = map(.x = lng, .f = ~paste0(.x, collapse = "-"))) %>%
  unnest(cols = lat) %>%
  unnest(cols = lng) %>%
  separate(col = "lat", into = c("lat1", "lat2", "lat3"), 
           sep = "-", remove = FALSE, fill = "right") %>%
  separate(col = "lng", into = c("lng1", "lng2", "lng3"), 
           sep = "-", remove = FALSE, fill = "right") %>%
  replace_na(list(lat1 = "", lat2 = "0", lat3 = "",
                  lng1 = "", lng2 = "0", lng3 = "")) %>%
  mutate(
    lat_ns = ifelse(str_detect(coord, "N"), "N", "S"),
    lng_ew = ifelse(str_detect(coord, "E"), "E", "W"),
    lat = paste0(lat1, "d", lat2, "'", lat3, 
                     ifelse(lat3 == "", "", "\""),
                     lat_ns), 
    lng = paste0(lng1, "d", lng2, "'", lng3,
                     ifelse(lng3 == "", "", "\""),
                     lng_ew), 
    lat = str_replace(string = lat, pattern = "d'N", replacement = "dN"),
    lng = str_replace(string = lng, pattern = "d'E", replacement = "dE"),
    lat = as.numeric(char2dms(lat)),
    lng = as.numeric(char2dms(lng))) %>%
  select(-matches(match = "[1-3]"), -lat_ns, -lng_ew) %>%
  rename(club_stadium_coord = coord, 
         club_stadium_lat = lat, 
         club_stadium_lbg = lng)

d3 <- d0 %>%
  left_join(d1) %>%
  left_join(d2)
write_csv(d3, "./data/clubs.csv")


# mutate(league_country = iconv(league_country, "latin1", "ASCII", sub=" "), 
#        league_country = sub("\\s+", " ", league_country))


##
##leagues country
##
# d3 <- d1 %>%
#   select(club_league2021) %>%
#   distinct() %>%
#   mutate(league2 = league,
#          league2 = ifelse(league == "Professional League", "Saudi Premier League", league2),
#          league2 = ifelse(league == "Championship", "Football_League_Championship", league2),
#          league2 = ifelse(league == "Süper Lig", "S?per_Lig", league2),
#          league2 = ifelse(league == "Superliga", "Danish_Superliga", league2),
#          league2 = ifelse(league == "Segunda División", "Segunda_Divisi?n", league2),
#          league2 = ifelse(league == "Prva HNL", "Croatian_First_Football_League", league2),
#          league2 = ifelse(league == "Parva Liga", "Bulgarian_First_Professional_League", league2),
#          league2 = ifelse(league == "Nemzeti Bajnokság I", "Nemzeti_Bajnoks?g_I", league2),
#          league2 = ifelse(league == "Nemzeti Bajnokság II", "Nemzeti_Bajnoks?g_II", league2),
#          league2 = ifelse(league == "Challenge League", "Swiss_Challenge_League", league2))
# 
# v5 <- NULL
# for(i in 1:nrow(df2)){
#   if(i == 4)
#     v6 <- "England"
#   if(!(i %in% c(4))){
#     url1 <- paste0("https://en.wikipedia.org/wiki/", df2$league2[i])
#     page1 <- read_html(url1)
#     v6 <- page1 %>%
#       html_nodes(".infobox") %>%
#       html_table() %>%
#       as.data.frame() %>% 
#       subset(X1 == "Country") %>%
#       .[,2]
#   }
#   v5 <- c(v5, v6)
#   message(df2$league[i])
#   message(v6)
# }
# 
# df2 <- df2 %>%
#   mutate(league_country = v5)
# 
# Encoding(df2$league_country) <- "UTF-8"
# 
# df2 <- df2 %>%
#   mutate(league_country = gsub("? ", "", league_country),
#          league_country = gsub("^[[:space:]]*", "", league_country),
#          league_country = gsub("\\(23 teams\\)", "", league_country))
# 
# df1 <- df1 %>%
#   left_join(df2)

write_csv(df1 , "clubs.csv")
