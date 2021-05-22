##
##cirlce1 clubs
##cirlce2 birthplace
##

library("dplyr")
library("readr")
library("circlize")

setwd("C:/Users/Guy/Dropbox/ecmig/")
setwd("C:/Users/gabel/Dropbox/ecmig/")

df0 <- read_csv("players.csv")
df1 <- read_csv("clubs.csv")
df2 <- read_csv("teams.csv")
df3 <- read_csv("birthplace.csv")

df5 <- df0 %>%
  left_join(df1) %>%
  rename(club_lat = lat, club_lon = lon) %>%
  select(-coords, -coords0) %>%
  left_join(df2, by = c("squad" = "Team")) %>%
  left_join(df3) %>%
  mutate(pob = ifelse(pob=="Russian Federation", "Russia", pob),
         pob = ifelse(pob=="Ireland", "Republic of Ireland", pob),
         pob = ifelse(pob=="Macedonia, Republic of", "Macedonia", pob)) 

df4 <- df5 %>%
  group_by(pob) %>%
  summarise(pob_n = n())

df5 <- df5 %>%
  left_join(df4) 

df6 <- df5 %>% 
  group_by(squad, pob, kit) %>%
  summarise(flow = n()) %>%
  select(pob, squad, flow, everything())

df7 <- df5 %>% 
  select(squad, kit) %>%
  distinct() %>%
  rename(pob = squad) 

df8 <- df6 %>%
  ungroup() %>%
  select(pob) %>%
  distinct() %>%
  filter(!(pob %in% df7$pob)) %>%
  # mutate(kit = "grey")
  mutate(kit = "transparent")

df9 <- df7 %>%
  bind_rows(df8) %>%
  left_join(df4) %>%
  mutate(not_attend = ifelse(kit == "transparent", TRUE, FALSE)) %>%
  arrange(desc(pob_n), pob) %>%
  mutate(reg1 = ifelse(not_attend, paste0(pob, " (", pob_n, ")"), pob),
         reg2 = ifelse(not_attend, "", paste0("(", pob_n, ")")),
         # reg1 = ifelse(reg1 == "Bosnia and Herzegovina", "Bosnia and Herz.", reg1),
         reg1 = ifelse(pob == "Congo, The Democratic Republic of the", 
                       sub(", The Democratic Republic of the", " D. R.", reg1), reg1),
         reg1 = ifelse(pob == "Republic of Ireland", sub("Republic of ", "", reg1), reg1))
         reg2 = ifelse(pob == "Northern Ireland", sub("Northern ", "", reg1), reg2), 
         reg1 = ifelse(pob == "Northern Ireland", "Northern", reg1), 
         reg2 = ifelse(pob == "Czech Republic", sub("Czech ", "", reg1), reg2), 
         reg1 = ifelse(pob == "Czech Republic", "Czech", reg1))
  

##
##chord diagram
##

circos.clear()
circos.par(start.degree = 90, gap.degree = 3, track.margin = c(-0.12, 0.12),
           cell.padding = c(0,0),
           points.overflow.warning = FALSE)
par(mar = rep(0, 4), bg = "black")

chordDiagram(x = df6, col = df6$kit, transparency = 0.1, grid.col = df9$kit,
             order = df9$pob, directional = 1, 
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
    reg1 = df9$reg1[df9$pob == sector.index]
    reg2 = df9$reg2[df9$pob == sector.index]
    kit = df9$kit[df9$pob == sector.index]

    if(kit == "transparent")
      circos.text(x = mean(xlim), y = 1, labels = reg1, col = "white",
                  facing = "clockwise", pos = 4, cex = 0.7, offset = 0)

    if(kit != "transparent"){
      circos.text(x = mean(xlim), y = ifelse(test = is.na(reg2) == 0, yes = 8, no = 6),
                  labels = reg1, col = "white", facing = "bending", cex = 0.8)
      circos.text(x = mean(xlim), y = 4.5, 
                  labels = reg2, col = "white", facing = "bending", cex = 0.8)
    }
  }
)
text(1.05,1,"Euro 2016 Squads", col="white", cex=1.4, pos=2)
text(1.05,0.94,"Birthplace to National Teams", col="white", cex=1, pos=2)

text(1.05,-1.00,c("Further Details: http://gjabel.wordpress.com"), pos=2, cex=0.8, col="white")
text(1.05,-1.04,c("Twitter: @guyabelguyabel"), pos=2, cex=0.8, col="white")

dev.print(png, "./plot_circle2.png", width = 20, height = 20, units = "cm", res=500)
file.show("./plot_circle2.png")

