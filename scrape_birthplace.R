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
library(geonames)
library(countrycode)

source("coords2country.R")

d <- read_csv("./data/players.csv")

d0 <- d %>%
  select(url_player) %>%
  distinct() %>%
  mutate(pob = NA, 
         pob_coord = NA)

for(i in 1:nrow(d0)){
  #birth place
  h1 <- paste0("https://en.wikipedia.org/", d0$url_player[i]) %>%
    read_html()
  
  pob <- h1 %>%
    html_nodes(".birthplace a") %>%
    html_attr("href") %>%
    str_subset(".wikipedia", negate = TRUE)
    
  if(length(pob) != 0) {
    if(str_detect(string = pob, pattern = "redlink=1")[1])
      next()
    d0$pob[i] <- pob[[1]]
    
    h2 <- paste0("https://en.wikipedia.org", d0$pob[i]) %>%
      read_html()
    
    cc <- h2 %>%
      html_nodes(".geo-dms")
    
    if(length(cc) != 0)
      d0$pob_coord[i] <- cc %>%
      .[1] %>%
      html_text()
  }
  if(i %% 10 == 0){
    message(i)
  }
}

d1 <- d0 %>%
  filter(!is.na(pob),
         !is.na(pob_coord)) %>%
  separate(col = "pob_coord", into = c("pob_lat", "pob_lng"), 
           sep = " ", remove = FALSE) %>%
  mutate(pob_lat = str_extract_all(string = pob_lat, pattern = "[0-9.]+"),
         pob_lng = str_extract_all(string = pob_lng, pattern = "[0-9.]+"),
         pob_lat = map(.x = pob_lat, .f = ~paste0(.x, collapse = "-")),
         pob_lng = map(.x = pob_lng, .f = ~paste0(.x, collapse = "-"))) %>%
  unnest(cols = pob_lat) %>%
  unnest(cols = pob_lng) %>%
  separate(col = "pob_lat", into = c("pob_lat1", "pob_lat2", "pob_lat3"), 
           sep = "-", remove = FALSE, fill = "right") %>%
  separate(col = "pob_lng", into = c("pob_lng1", "pob_lng2", "pob_lng3"), 
           sep = "-", remove = FALSE, fill = "right") %>%
  replace_na(list(pob_lat1 = "", pob_lat2 = "0", pob_lat3 = "",
                  pob_lng1 = "", pob_lng2 = "0", pob_lng3 = "")) %>%
  mutate(
    pob_lat_ns = ifelse(str_detect(pob_coord, "N"), "N", "S"),
    pob_lng_ew = ifelse(str_detect(pob_coord, "E"), "E", "W"),
    pob_lat = paste0(pob_lat1, "d", pob_lat2, "'", pob_lat3, 
                     ifelse(pob_lat3 == "", "", "\""),
                     pob_lat_ns), 
    pob_lng = paste0(pob_lng1, "d", pob_lng2, "'", pob_lng3,
                     ifelse(pob_lng3 == "", "", "\""),
                     pob_lng_ew), 
    pob_lat = str_replace(string = pob_lat, pattern = "d'N", replacement = "dN"),
    pob_lng = str_replace(string = pob_lng, pattern = "d'E", replacement = "dE"),
    pob_lat = as.numeric(char2dms(pob_lat)),
    pob_lng = as.numeric(char2dms(pob_lng)),
    cob_alpha = coords2country(cbind(pob_lng, pob_lat)),
    cob = countrycode(sourcevar = cob_alpha, 
                      origin = "iso3c", destination = "country.name"),
    cob = ifelse(cob_alpha == "KOS", "Kosovo", cob)) %>%
  select(-matches(match = "[1-3]"), -pob_lat_ns, -pob_lng_ew) %>%
  rename(cob_alpha3 = cob_alpha)

##
## England, Wales, Scotland, NI
##

d2 <- d1 %>%
  filter(cob_alpha3 == "GBR") %>%
  mutate(cob_uk = NA)

# options(geonamesUsername="gjabel")
for(i in 1:nrow(d2)){
  d2$cob_uk[i] <- GNcountrySubdivision(lat = d2$pob_lat[i],
                                       lng = d2$pob_lng[i], 
                                       radius = 5)$adminName1
  message(i)
}

d3 <- d1 %>%
  left_join(d2)
  
write_csv(d3, "./data/birthplace.csv")







