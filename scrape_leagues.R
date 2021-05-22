library("rvest")
library("dplyr")
library("readr")

setwd("C:/Users/Guy/Dropbox/ecmig/")

df0 <- read_csv("./clubs.csv")

##
##league coefficents
##
df1 <- df0 %>%
  select(league_country) %>%
  distinct()

url0 <- paste0("https://en.wikipedia.org/wiki/UEFA_coefficient#Current_ranking")
page0 <- read_html(url0)

df2 <- page0 %>%
  html_nodes("table.sortable.wikitable") %>%
  .[[1]] %>%
  html_table(fill = TRUE) %>%
  as_data_frame() %>%
  select(1,4) 

names(df2) <- c("uefa_coef", "league")

df2 <- df2 %>%
  separate(col = league , into = c("league_country", "extra"), sep = " \\(") %>%
  select(-extra)

df3 <- df2 %>%
  left_join(df1) %>%
  mutate(league_country = iconv(league_country, "latin1", "ASCII", sub=" "), 
         league_country = gsub("\\s+", " ", league_country))

# df3 %>% filter(league_country=="Republic of Ireland")
# df3 %>% slice(41)

write_csv(df3, "leagues.csv")