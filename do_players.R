library("rvest")
library("dplyr")
library("readr")

setwd("C:/Users/Guy/Dropbox/ecmig/")

url0 <- read_html("https://en.wikipedia.org/wiki/UEFA_Euro_2016_squads", encoding = "UTF-8") 

#squad tables
df0 <- url0 %>%
  html_nodes("table.sortable.wikitable.plainrowheaders") %>%
  html_table() %>%
  lapply(as_data_frame) %>%
  bind_rows()
Encoding(df0$Club) <- "UTF-8"
Encoding(df0$Player) <- "UTF-8"

##
##club links
##
v0 <- url0 %>%
  html_nodes("td:nth-child(7) a:nth-child(2)") %>%
  html_attr("href")

##
##squad names
##
v1 <- url0 %>%
  html_nodes("h3 .mw-headline") %>%
  html_text() %>%
  .[1:24] %>%
  rep(each = 23)


##
##player pages
##
v2 <- url0 %>%
  html_nodes("table.sortable.wikitable.plainrowheaders") %>%
  html_nodes("th a") %>%
  html_attr("href")

v2 <- v2[v2!="/wiki/Captain_(association_football)"]


#combine
df0 <- df0 %>% 
  mutate(club_link = paste0("https://en.wikipedia.org", v0), 
         squad = v1,
         player_link = paste0("https://en.wikipedia.org", v2) )

write_csv(df0 , "players.csv")
