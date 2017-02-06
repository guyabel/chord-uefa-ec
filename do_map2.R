##
##map1 clubs
##map1 birthplace
##

library("dplyr")
library("readr")
library("ggplot2")
library("geosphere")

setwd("C:/Users/Guy/Documents/GitHub/ecmig/")

df0 <- read_csv("players.csv")
df1 <- read_csv("clubs.csv")
df2 <- read_csv("teams.csv")
df3 <- read_csv("birthplace.csv")

df4 <- df0 %>%
  left_join(df1) %>%
  rename(club_lat = lat, club_lon = lon) %>%
  select(-coords, -coords0) %>%
  left_join(df2, by = c("squad" = "Team")) %>%
  left_join(df3) 


options(expressions=500000)
world <- map_data("world")

g1 <- ggplot(data = world) +
  coord_map("globular", xlim = c(-10, 36), ylim = c(35, 67)) +
  geom_polygon(aes(x=long, y=lat, group=group), color = "darkgrey", fill = "black") +
  theme_classic() +
  theme(panel.background = element_rect(fill = "black"),
        axis.ticks=element_blank(),
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
g1

for(i in 1:nrow(df4)){
  df5 <- df4 %>% 
    slice(i) %>%
    mutate(kit = adjustcolor(kit, alpha = 0.6))
  df6 <- gcIntermediate(p1 = c(df5$pob_lon, df5$pob_lat),
                        p2 = c(df5$cap_lon, df5$cap_lat),
                        n = 50, addStartEnd=TRUE) %>%
    as.data.frame() %>%
    as_data_frame()
  
  g1 <- g1 + 
    geom_point(data = df5, aes(x = pob_lon, y = pob_lat), color = df5$kit ) +
    geom_point(data = df5, aes(x = cap_lon, y = cap_lat), color = df5$kit )
  if(nrow(df6)>3)
    g1 <- g1 + geom_line(data = df6, aes(x = lon, y = lat), color = df5$kit ) 
}
gc()
g1 + 
  theme(title=element_text(size=25,face="bold")) +
  ggtitle("Euro 2016", subtitle="Place of Birth to National Capital") +
  labs(caption = "Further Details: http://gjabel.wordpress.com  Twitter: @guyabelguyabel")

ggsave(filename = "./plot_pob1.png", width = 50, height = 50, units = "cm")
file.show(filename = "./plot_pob1.png")

g1 + 
  coord_map("globular", xlim = range(df4$pob_lon), ylim = range(df4$pob_lat)) +
  theme(title=element_text(size=25,face="bold")) +
  ggtitle("Euro 2016", subtitle="Place of Birth to National Capital") +
  labs(caption = "Further Details: http://gjabel.wordpress.com  Twitter: @guyabelguyabel")
ggsave(filename = "./plot_pob2.png", width = 50, height = 50, units = "cm")
file.show(filename = "./plot_pob2.png")