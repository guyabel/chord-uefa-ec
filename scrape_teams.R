library("rvest")
library("dplyr")
library("tidyr")
library("readr")
# require(devtools)
# install_github("ramnathv/rblocks")
# install_github("woobe/rPlotter")
library("rPlotter")
library("sp")

setwd("C:/Users/Guy/Dropbox/ecmig/")

##
##countries
##

url2 <- "https://en.wikipedia.org/wiki/UEFA_Euro_2016"
page2 <- read_html(url2) 

df2 <- page2 %>%
  html_nodes("table.sortable.wikitable") %>%
  .[[2]] %>%
  html_table() %>%
  as_data_frame()

Encoding(df2$Team) <- "UTF-8"

df2 <- df2 %>%
  mutate(Team = gsub("Â ", "", Team),
         Team = gsub("^[[:space:]]*", "", Team),
         team_link = paste0("https://en.wikipedia.org/wiki/", Team, "_national_football_team"))
Encoding(df2$`Base camp`) <- "UTF-8"
df2 <- df2 %>%
  mutate(base_link = paste0("https://en.wikipedia.org/wiki/", `Base camp`), 
         base_link = ifelse(Team == "England", paste0(base_link, ",_Oise"), base_link),
         base_link = ifelse(Team == "Hungary", paste0(base_link, ",_Var"), base_link))

##
##kit colors
##
kit0 <- NULL
for(i in 1:nrow(df2)){
  url0 <- df2$team_link[i]
  page0 <- read_html(url0)
  #kit colour
  img0 <- page0 %>%
    html_nodes(".toccolours td:nth-child(1) div:nth-child(3) img") %>%
    html_attr("src")
  
  kit1 <- extract_colours(paste0("https:", img0), num_col = 1, rsize = 10)
  pie(rep(1, length(kit1)), col = kit1, main = df2$Team[i])
  kit0 <- c(kit0, kit1)
}
df2 <- df2 %>% 
  mutate(kit = kit0)

##
##base geo
##
v0 <- NULL
for(i in 1:nrow(df2)){
  url1 <- df2$base_link[i]
  page1 <- read_html(url1)
  v1 <- page1 %>%
    html_nodes(".geo-dms") %>%
    .[1] %>%
    html_text()
  v0 <- c(v0, v1)
  message(i)
}
df2 <- df2 %>%
  mutate(coords0 = v0,
         coords = gsub(" ", "  ", coords0),
         coords = gsub("Â°", "d ", coords),
         coords = gsub("â???²", "' ", coords),
         coords = gsub("â???³", "\" ", coords)) %>%
  separate(col = "coords", into = c("base_lat", "base_lon"), sep = "  ", remove = FALSE) %>%
  mutate(base_lat = as.numeric(char2dms(base_lat)),
         base_lon = as.numeric(char2dms(base_lon)))


##
##Capital geo
##
v0 <- NULL
for(i in 1:nrow(df2)){
  url1 <- paste0("https://en.wikipedia.org/wiki/", df2$Team[i])
  page1 <- read_html(url1)
  v1 <- page1 %>%
    html_nodes(".vcard") %>%
    html_nodes(".geo-dms") %>%
    .[1] %>%
    html_text()
  v0 <- c(v0, v1)
  message(i)
  message(v1)
}
df2 <- df2 %>%
  mutate(coords0 = v0,
         coords = gsub(" ", "  ", coords0),
         coords = gsub("Â°", "d ", coords),
         coords = gsub("â???²", "' ", coords),
         coords = gsub("â???³", "\" ", coords)) %>%
  separate(col = "coords", into = c("cap_lat", "cap_lon"), sep = "  ", remove = FALSE) %>%
  mutate(cap_lat = as.numeric(char2dms(cap_lat)),
         cap_lon = as.numeric(char2dms(cap_lon)))

write_csv(df2, "./teams.csv")