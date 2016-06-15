library("rvest")
library("dplyr")
library("tidyr")
library("sp")

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

#club links
v0 <- url0 %>%
  html_nodes("td:nth-child(7) a:nth-child(2)") %>%
  html_attr("href")

#squad names
v1 <- url0 %>%
  html_nodes("h3 .mw-headline") %>%
  html_text() %>%
  .[1:24] %>%
  rep(each = 23)

#combine
df0 <- df0 %>% 
  mutate(club_link = paste0("https://en.wikipedia.org", v0), 
         squad = v1)

#club infomation
df1 <- df0 %>%
  select(Club, club_link) %>%
  distinct() %>%
  arrange(Club)
v2 <- NULL
v3 <- NULL
for(i in 1:nrow(df1)){
  url1 <- read_html(df1$club_link[i])
  #league
  v4 <- url1 %>%
    html_nodes(".vcard") %>%
    .[[1]] %>%
    html_table(fill = TRUE) %>% 
    subset(X1=="League") %>% 
    .[[2]]
  v2 <- c(v2, v4)
  #ground wiki page
  v5 <- url1 %>%
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

#club grounds geo
v4 <- NULL
for(i in 1:nrow(df1)){
  url2 <- read_html(df1$stadium_link[i])
  v5 <- 
    url2 %>%
    # html_nodes(".longitude") %>%
    # html_nodes("#coordinates .geo-dms")
    # html_nodes("#coordinates") %>%
    html_nodes(".geo-dms") %>%
    .[1] %>%
    html_text()
  v4 <- c(v4, v5)
  message(i)
}

df2 <- df1 %>%
  mutate(coords0 = v4,
         coords = gsub(" ", "  ", coords0),
         coords = gsub("Â°", "d ", coords),
         coords = gsub("â???²", "' ", coords),
         coords = gsub("â???³", "\" ", coords)) %>%
  separate(col = "coords", into = c("lat", "lon"), sep = "  ", remove = FALSE) %>%
  mutate(lat = as.numeric(char2dms(lat)),
         lon = as.numeric(char2dms(lon)))

df0


url1 <- read_html("https://en.wikipedia.org/wiki/UEFA_Euro_2016", encoding = "UTF-16") 
url1 %>%
  html_nodes("table") %>%
  .[[10]] %>%
  # html_table() %>%
  html_nodes(xpath = "./a")
  html_attr("ahref")
  html_attr()

  html_nodes("table.sortable.wikitable.plainrowheaders")


df0 <- lst0 %>%
  html_table() %>%
  as_data_frame()
  %>%
  .[[1]]
  unlist() %>%
  as_data_frame()
  
  html_structure()
    html_nodes(xpath = '.sortable wikitable plainrowheaders jquery-tablesorter') %>% 
  html_table

  html_nodes("table") %>% 
  html_structure()
  .[[1]] %>%  html_children() %>% html_table()
  html_nodes("table")
  html_nodes("h3")

%>%
  html_table()

lst0[1]
  html_nodes(".sortable wikitable plainrowheaders jquery-tablesorter") %>% 
  html_table(fill = TRUE)
  .[[1]]
    html_nodes("sortable wikitable plainrowheaders jquery-tablesorter") %>%
    html_nodes("span") %>%
    html_children() 
  
  
url <- 

  library("XML")
doc <- htmlParse(url0)
tableNodes = getNodeSet(doc, "//table")

library("dplyr")
df0 <- NULL
for(i in 1:24){
  df1 <- readHTMLTable(tableNodes[[i]])
  
  df0 <- df0 %>% 
    bind_rows(df1) 
}
names(df0)<-c("number","position","player","dob","caps","club")

df0 <- 
  %>%
  html_nodes('.td') 

%>%
  html_table()
