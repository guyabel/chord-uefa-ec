##
##map1 clubs
##map1 birthplace
##

library("dplyr")
library("readr")
library("ggplot2")
library("geosphere")

setwd("C:/Users/Guy/Dropbox/ecmig/")
setwd("C:/Users/gabel/Dropbox/ecmig/")

df0 <- read_csv("players.csv")
df1 <- read_csv("clubs.csv")
df2 <- read_csv("teams.csv")

df3 <- df0 %>%
  left_join(df1) %>%
  rename(club_lat = lat, club_lon = lon) %>%
  select(-coords, -coords0) %>%
  left_join(df2, by = c("squad" = "Team"))


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
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
g2 <- g1
g1

for(i in 1:nrow(df3)){
  df4 <- df3 %>% 
    slice(i) %>%
    mutate(kit = adjustcolor(kit, alpha = 0.6))
  df5 <- gcIntermediate(p1 = c(df4$club_lon, df4$club_lat),
                        p2 = c(df4$base_lon, df4$base_lat),
                        n = 50, addStartEnd=TRUE) %>%
    as.data.frame() %>%
    as_data_frame()
  
  df6 <- gcIntermediate(p1 = c(df4$club_lon, df4$club_lat),
                        p2 = c(df4$cap_lon, df4$cap_lat),
                        n = 50, addStartEnd=TRUE) %>%
    as.data.frame() %>%
    as_data_frame()
  
  g1 <- g1 + 
    geom_point(data = df4, aes(x = club_lon, y = club_lat), color = df4$kit ) +
    geom_point(data = df4, aes(x = base_lon, y = base_lat), color = df4$kit ) +
    geom_line(data = df5, aes(x = lon, y = lat), color = df4$kit ) 

  g2 <- g2 + 
    geom_point(data = df4, aes(x = club_lon, y = club_lat), color = df4$kit ) +
    geom_point(data = df4, aes(x = cap_lon, y = cap_lat), color = df4$kit ) +
    geom_line(data = df6, aes(x = lon, y = lat), color = df4$kit ) 
}
gc()
g1 + 
  theme(title=element_text(size=25,face="bold")) +
  ggtitle("Euro 2016", subtitle="Players Clubs to National Team Base") +
  labs(caption = "Further Details: http://gjabel.wordpress.com  Twitter: @guyabelguyabel")

ggsave(filename = "./plot_base.png", width = 50, height = 50, units = "cm")
file.show(filename = "./plot_base.png")

g2 + 
  theme(title=element_text(size=25,face="bold")) +
  ggtitle("Euro 2016", subtitle="Players Clubs to National Capital") +
  labs(caption = "Further Details: http://gjabel.wordpress.com  Twitter: @guyabelguyabel")
ggsave(filename = "./plot_cap.png", width = 50, height = 50, units = "cm")
file.show(filename = "./plot_cap.png")


g2 + 
  coord_map("globular", xlim = range(df3$club_lon[df3$club_lat>0]), ylim = range(df3$club_lat[df3$club_lat>0])) +
  theme(title=element_text(size=25,face="bold")) +
  ggtitle("Euro 2016", subtitle="Players Clubs to National Capital") +
  labs(caption = "Further Details: http://gjabel.wordpress.com  Twitter: @guyabelguyabel")
ggsave(filename = "./plot_cap2.png", width = 50, height = 20, units = "cm")
file.show(filename = "./plot_cap2.png")