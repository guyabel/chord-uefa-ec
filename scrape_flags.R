##
## scrape_players: players in squads
## scrape_flag: national team flags
## scrape_colours: national team kits
## scrape_comp: competition teams and logos
##

library(tidyverse)
library(magick)

w <- read_csv("./data/wiki_players.csv", guess_max = 1e5)

n0 <- w %>%
  select(nat_team, nat_team_alpha3) %>%
  distinct() %>%
  rename(label = nat_team, 
         alpha3 = nat_team_alpha3)

d0 <- w %>%
  select(club_country_harm, club_alpha3, club_country_flag, year) %>%
  filter(club_alpha3 %in% n0$alpha3) %>%
  group_by_at(1:3) %>%
  mutate(year_min = min(year), 
         year_max = max(year),
         years = paste0(unique(sort(year)), collapse = ",")) %>%
  ungroup() %>%
  select(-year) %>%
  distinct() %>%
  rename(label = club_country_harm, 
         alpha3 = club_alpha3,
         flag_url = club_country_flag) %>%
  drop_na() %>%
  arrange(label) %>%
  mutate(label = ifelse(str_detect(string = flag_url, pattern = "Wales"), 
                        "Wales", label),
         alpha3 = ifelse(str_detect(string = flag_url, pattern = "Wales"), 
                         "GB-WLS", alpha3),
         flag_url = paste0("https:", flag_url)) 

# any countries not that never had any national team players
# in their league?
n0 %>%
  filter(!alpha3 %in% unique(d0$alpha3)) 

d1 <- n0 %>% 
  filter(!alpha3 %in% unique(d0$alpha3)) %>%
  mutate(
    flag_url = c(
      "https://upload.wikimedia.org/wikipedia/commons/4/45/Flag_of_Ireland.svg",
      "https://upload.wikimedia.org/wikipedia/commons/3/33/Flag_of_the_CIS_%28UEFA_Euro_1992%29.svg",
      "https://upload.wikimedia.org/wikipedia/commons/4/43/Flag_of_Northern_Ireland_%281953%E2%80%931972%29.svg",
      "https://upload.wikimedia.org/wikipedia/commons/c/ce/Flag_of_Iceland.svg"
    ),
    years = c(
      "1988,2012,2016",
      "1992",
      "2016",
      "2016"),
    year_min = c(1988, 1992, 2016, 2016),
    year_max = c(2016, 1992, 2016, 2016)
    )

# use most upto date flag
d2 <- d0 %>%
  bind_rows(d1) %>%
  arrange(alpha3, year_min) %>%
  group_by(alpha3) %>%
  slice(n())

b <- image_blank(width = 100, height = 75, color = "grey40")
for(i in 1:nrow(d2)){
  message(d2$alpha3[i])
  f <- image_read_svg(path = d2$flag_url[i])
  f %>%
    image_resize("100x75") %>%
    image_composite(image = b, composite_image = ., gravity = "center") %>%
    image_write(path = paste0("./flag/",d2$alpha3[i], ".png"))
}