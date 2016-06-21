library("rvest")
library("dplyr")
library("tidyr")
library("sp")
library("geonames")
library("ISOcodes")

setwd("C:/Users/Guy/Dropbox/ecmig/")

df0 <- read_csv("players.csv")
data("ISO_3166_1")

##
##place of birth
##

v0 <- NULL
v1 <- NULL
v2 <- NULL
for(i in 1:nrow(df0)){
  #birth place
  url1 <- df0$player_link[i]
  page1 <- read_html(url1)
  v0 <- page1 %>%
    html_nodes(".birthplace a") %>%
    html_attr("href") %>%
    .[[1]]
  
  #birth place long and lat
  url2 <- paste0("https://en.wikipedia.org", v0)
  page2 <- read_html(url2)
  v1 <- page2 %>%
    html_nodes(".geo-dms") %>%
    .[1] %>%
    html_text()
  v2 <- c(v2, v1)
  message(v0)
}

df1 <- df0 %>%
  select(player_link) %>%
  mutate(coords0 = v2,
         coords = gsub(" ", "  ", coords0),
         coords = gsub("°", "d ", coords),
         coords = gsub("′", "' ", coords),
         coords = gsub("″", "\" ", coords)) %>%
  separate(col = "coords", into = c("pob_lat", "pob_lon"), sep = "  ", remove = FALSE) %>%
  mutate(pob_lat = as.numeric(char2dms(pob_lat)),
         pob_lon = as.numeric(char2dms(pob_lon)))

v3 <- NULL
for(i in 1:nrow(df1)){
  v4 <- GNcountryCode(df1$pob_lat[i],df1$pob_lon[i], radius = 5)$countryCode
  v3 <- c(v3, v4)
  message(v4)
}

df2 <- df1 %>%
  select(-contains("coords")) %>%
  mutate(pob_code = v3) %>%
  left_join(ISO_3166_1 %>% select(Alpha_2, Name), by = c("pob_code" = "Alpha_2")) %>%
  rename(pob = Name) %>%
  mutate(pob = ifelse(pob_code == "XK", yes = "Kosovo", no = pob))

df3 <- df2 %>%
  filter(pob == "United Kingdom")

v5 <- NULL
for(i in 1:nrow(df3)){
  v6 <- GNcountrySubdivision(df3$pob_lat[i],df3$pob_lon[i], radius = 5)$adminName1
  v5 <- c(v5, v6)
  message(v6)
}

df3 <- df3 %>%
  mutate(pob_uk = v5)

df2 <- df2 %>%
  left_join(df3) %>%
  mutate(pob = ifelse(is.na(pob_uk), pob, pob_uk))

write_csv(df2, "./birthplace.csv")







