library("rvest")
library("dplyr")
library("readr")
library("tidyr")
library("sp")

setwd("C:/Users/Guy/Dropbox/ecmig/")

df0 <- read_csv("players.csv")

##
##club infomation
##

df1 <- df0 %>%
  select(Club, club_link) %>%
  distinct() %>%
  arrange(Club)

v2 <- NULL
v3 <- NULL
for(i in 1:nrow(df1)){
  url1 <- df1$club_link[i]
  page1 <- read_html(url1)
  #league
  v4 <- page1 %>%
    html_nodes(".vcard") %>%
    .[[1]] %>%
    html_table(fill = TRUE) %>% 
    subset(X1=="League") %>% 
    .[[2]]
  v2 <- c(v2, v4)
  #ground wiki page
  v5 <- page1 %>%
    html_nodes(".vcard") %>%
    .[[1]] %>%
    html_nodes(".label a") %>%
    .[1] %>%
    html_attr("href")
  v3 <- c(v3, v5)
  message(df1 %>% slice(i))
}
df1 <- df1 %>%
  mutate(league = v2,
         stadium_link = paste0("https://en.wikipedia.org", v3))

##
##club grounds geo
##

v4 <- NULL
for(i in 1:nrow(df1)){
  url1 <- df1$stadium_link[i]
  page1 <- read_html(url1)
  v5 <- page1 %>%
    html_nodes(".geo-dms") %>%
    .[1] %>%
    html_text()
  v4 <- c(v4, v5)
  message(df1$stadium_link[i])
}
df1 <- df1 %>%
  mutate(coords0 = v4,
         coords = gsub(" ", "  ", coords0),
         coords = gsub("Â°", "d ", coords),
         coords = gsub("â€²", "' ", coords),
         coords = gsub("â€³", "\" ", coords)) %>%
  separate(col = "coords", into = c("lat", "lon"), sep = "  ", remove = FALSE) %>%
  mutate(lat = as.numeric(char2dms(lat)),
         lon = as.numeric(char2dms(lon)))


# mutate(league_country = iconv(league_country, "latin1", "ASCII", sub=" "), 
#        league_country = sub("\\s+", " ", league_country))


##
##leagues country
##
df2 <- df1 %>%
  select(league) %>%
  distinct() %>%
  mutate(league2 = league,
         league2 = ifelse(league == "Professional League", "Saudi Premier League", league2),
         league2 = ifelse(league == "Championship", "Football_League_Championship", league2),
         league2 = ifelse(league == "SÃ¼per Lig", "Süper_Lig", league2),
         league2 = ifelse(league == "Superliga", "Danish_Superliga", league2),
         league2 = ifelse(league == "Segunda DivisiÃ³n", "Segunda_División", league2),
         league2 = ifelse(league == "Prva HNL", "Croatian_First_Football_League", league2),
         league2 = ifelse(league == "Parva Liga", "Bulgarian_First_Professional_League", league2),
         league2 = ifelse(league == "Nemzeti BajnoksÃ¡g I", "Nemzeti_Bajnokság_I", league2),
         league2 = ifelse(league == "Nemzeti BajnoksÃ¡g II", "Nemzeti_Bajnokság_II", league2),
         league2 = ifelse(league == "Challenge League", "Swiss_Challenge_League", league2))

v5 <- NULL
for(i in 1:nrow(df2)){
  if(i == 4)
    v6 <- "England"
  if(!(i %in% c(4))){
    url1 <- paste0("https://en.wikipedia.org/wiki/", df2$league2[i])
    page1 <- read_html(url1)
    v6 <- page1 %>%
      html_nodes(".infobox") %>%
      html_table() %>%
      as.data.frame() %>% 
      subset(X1 == "Country") %>%
      .[,2]
  }
  v5 <- c(v5, v6)
  message(df2$league[i])
  message(v6)
}

df2 <- df2 %>%
  mutate(league_country = v5)

Encoding(df2$league_country) <- "UTF-8"

df2 <- df2 %>%
  mutate(league_country = gsub("Â ", "", league_country),
         league_country = gsub("^[[:space:]]*", "", league_country),
         league_country = gsub("\\(23 teams\\)", "", league_country))

df1 <- df1 %>%
  left_join(df2)

write_csv(df1 , "clubs.csv")


