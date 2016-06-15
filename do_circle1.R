library("dplyr")
library("readr")
library("circlize")

setwd("C:/Users/Guy/Dropbox/ecmig/")

df0 <- read_csv("players.csv")
df1 <- read_csv("clubs.csv")
df2 <- read_csv("teams.csv")
df3 <- read_csv("leagues.csv")

df4 <- df0 %>%
  left_join(df1) %>%
  rename(club_lat = lat, club_lon = lon) %>%
  select(-coords, -coords0) %>%
  left_join(df2, by = c("squad" = "Team"))

df5 <- df4 %>% 
  mutate(league_country = iconv(league_country, "latin1", "ASCII", sub=" "), 
         league_country = gsub("\\s+", " ", league_country)) %>%
  group_by(squad, league_country, kit) %>%
  summarise(flow = n()) %>%
  select(league_country, squad, flow, everything())

df6 <- df4 %>% 
  select(squad, kit) %>%
  distinct() %>%
  rename(league_country = squad) 

df7 <- df5 %>%
  ungroup() %>%
  select(league_country) %>%
  distinct() %>%
  filter(!(league_country %in% df6$league_country)) %>%
  # mutate(kit = "grey")
  mutate(kit = "transparent")

df8 <- df6 %>%
  bind_rows(df7) %>%
  left_join(df3) %>%
  mutate(not_attend = ifelse(kit == "transparent", TRUE, FALSE)) %>%
  arrange(not_attend, uefa_coef) %>%
  mutate(reg1 = league_country,
         reg2 = NA,
         reg1 = ifelse(reg1 == "Australia New Zealand", "Australia", reg1),
         reg1 = ifelse(reg1 == "Saudi Arabia", "Saudi Arab.", reg1),
         reg1 = ifelse(reg1 == "People's Republic of China", "China", reg1),
         reg1 = ifelse(reg1 == "Republic of Ireland", "Ireland", reg1),
         reg1 = ifelse(reg1 == "Northern Ireland", "Northern", reg1),
         reg2 = ifelse(league_country == "Northern Ireland", "Ireland", reg2),
         reg1 = ifelse(reg1 == "Czech Republic", "Czech", reg1),
         reg2 = ifelse(league_country == "Czech Republic", "Republic", reg2))
  
df8 <- df8 %>%
  filter(reg1 != "Australia") %>%
  bind_rows(df8 %>% filter(reg1 == "Australia")) %>%
  mutate(order = 1:n())



##
##chord diagram
##

circos.clear()
circos.par(start.degree = 90, gap.degree = 3, track.margin = c(-0.1, 0.1),
           cell.padding = c(0,0),
           points.overflow.warning = FALSE)
par(mar = rep(0, 4), bg = "black")

chordDiagram(x = df5, col = df5$kit, transparency = 0.1, grid.col = df8$kit,
             order = df8$league_country, directional = 1, 
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
             annotationTrack = "grid", annotationTrackHeight = c(0.01, 0.01),
             link.arr.type = "big.arrow",link.sort = TRUE, link.largest.ontop = TRUE)

##
##add in labels and axis
##

circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    reg1 = df8$reg1[df8$league_country == sector.index]
    reg2 = df8$reg2[df8$league_country == sector.index]
    kit = df8$kit[df8$league_country == sector.index]

    if(kit == "transparent")
      circos.text(x = mean(xlim), y = 1, labels = reg1, col = "white",
                  facing = "clockwise", pos = 4, cex = 0.8, offset = 0)

    if(kit != "transparent"){
      circos.text(x = mean(xlim), y = ifelse(test = is.na(reg2) == 0, yes = 8, no = 6),
                  labels = reg1, col = "white", facing = "bending", cex = 0.8)
      circos.text(x = mean(xlim), y = 4.5, 
                  labels = reg2, col = "white", facing = "bending", cex = 0.8)
    }
  }
)
text(1.05,1,"Euro 2016 Squads", col="white", cex=1.4, pos=2)
text(1.05,0.94,"Leagues to National Teams", col="white", cex=1, pos=2)

text(1.05,-1.00,c("Further Details: http://gjabel.wordpress.com"), pos=2, cex=0.8, col="white")
text(1.05,-1.04,c("Twitter: @guyabelguyabel"), pos=2, cex=0.8, col="white")

dev.print(png, "./plot_circle.png", width = 20, height = 20, units = "cm", res=500)
file.show("./plot_circle.png")

