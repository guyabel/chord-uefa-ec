library(tidyverse)
library(tweenr)
library(circlize)
library(migest)
library(animation)
library(magick)
library(grid)

##
## data
##
x1 <- read_csv("./data/wfnet_players.csv")
x2 <- read_csv("./data/wfnet_leagues.csv")
x3 <- read_csv("./data/wiki_colours.csv")
x4 <- read_csv("./data/wiki_comp.csv")

# league to national team data frame
d1 <- x1 %>%
  filter(!staff) %>%
  # squad sizes pre 1980 seem erratic / vary by source
  # filter(year >= 1980) %>%
  left_join(x2) %>%
  select(year, team, league, contains("alpha3")) %>%
  replace_na(list(league = "No Club")) %>%
  group_by(league, team, year, league_alpha3, team_alpha3) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  left_join(x3) %>%
  select(-url_team) %>%
  arrange(year)

t0 <- d1 %>%
  select(contains("team"), contains("kit")) %>%
  distinct() %>%
  mutate(team = factor(x = team, levels = unique(d1$team))) %>%
  arrange(team) %>%
  mutate(team = as.character(team)) %>%
  rename(lab = team, 
         alpha3 = team_alpha3)

l0 <- d1 %>%
  select(contains("league")) %>%
  distinct() %>%
  filter(!(league %in% t0$lab)) %>%
  mutate(kit_shirt = "transparent", 
         kit_shorts = "transparent", 
         kit_socks = "transparent",
         league = factor(x = league, levels = unique(d1$league)),
         league = fct_rev(league)) %>%
  arrange(league) %>%
  mutate(league = as.character(league)) %>%
  rename(lab = league,
         alpha3 = league_alpha3)


d0 <- t0 %>%
  bind_rows(l0) %>%
  mutate(label = str_wrap(string = lab, width = 11)) %>%
  separate(col = label, into = c("lab1", "lab2"), sep = "\n", 
           fill = "right", remove = FALSE) %>%
  mutate(y = ifelse(test = !is.na(lab2), yes = 1, no = 0.6),
         label = ifelse(test = alpha3 %in% c("DEU", "GB-NIR", "SCG"),
                        label, lab),
         label = ifelse(label == "Czechoslovakia", "Czecho-\nslovakia", label))
                  

z <- expand_grid(year = unique(sort(d1$year)),
                 team = d0$lab,
                 league = d0$lab) %>%
  mutate(n = ifelse(team == league, 0.01, 0),
         kit_shirt = "transparent", 
         kit_shorts = "transparent", 
         kit_socks = "transparent",
         kit_away = "transparent")

d2 <- d1 %>%
  bind_rows(z) %>%
  group_by(year, team, league) %>%
  filter(n == max(n)) %>%
  ungroup() %>%
  arrange(year) %>%
  select(-contains("alpha3"))


m0 <- d2 %>%
  # mutate(n = ifelse(n == 0.1, 0, n)) %>%
  group_by(year) %>%
  complete(league, team, fill = list(n = 0)) %>%
  sum_turnover(orig_col = "league", dest_col = "team",
               flow_col = "n", drop_diagonal = FALSE,
               include_net = FALSE) %>%
  mutate(tot = tot_in + tot_out) %>%
  group_by(region) %>%
  filter(tot == max(tot)) %>%
  ungroup() %>%
  select(-year) %>%
  distinct() %>%
  select(region, tot) %>%
  deframe()

# tween
d3 <- d2 %>%
  mutate(corridor = paste(league, team, sep = " -> ")) %>%
  select(-league, -team, -contains("kit"), kit_shirt) %>%
  mutate(ease = "linear") %>%
  tween_elements(time = "year", group = "corridor", ease = "ease", 
                 nframes = diff(range(d1$year)) * 4) %>%
  as_tibble() %>%
  separate(col = .group, into = c("league", "team"), sep = " -> ") %>%
  relocate(league, team, n)





pdf(file = "./plot/temp.pdf", height = 7, width = 7, useDingbats = FALSE)
# for(f in 48){
for(f in unique(d3$.frame)){
# for(f in max(d3$.frame)){
# for(f in min(d3$.frame)){
  d4 <- d3 %>%
    filter(.frame == f)
    # filter(year ==  1976)
    # filter(year ==  1976,
    #        kit_shirt != "#FEFFFE00")
  # if(!d4$year[1] %in% x4$year)
  #   next()
  
  par(mar = rep(0, 4), bg = "grey40", lheight = 0.8)
  circos.clear()
  circos.par(track.margin=c(0.01, -0.01), points.overflow.warning = FALSE,
             start.degree = 90, gap.degree = 3)
  
  # plot the chord diagram
  chordDiagram(
    x = select(d4, team, league, n),
    col = pull(d4, kit_shirt),
    # link.border = pull(d4, kit_shorts),
    # link.border = pull(d4, kit_away),
    order = d0 %>%
      filter(lab %in% unique(c(d4$team, d4$league))) %>%
      pull(lab),
    grid.col = d0 %>%
      select(lab, kit_shirt) %>%
      deframe(),
    transparency = 0.1,
    directional = -1, direction.type = c("diffHeight", "arrows"),
    link.arr.type = "big.arrow", diffHeight  = -0.02,
    link.sort = TRUE,
    link.largest.ontop = TRUE,
    annotationTrack = "grid",
    annotationTrackHeight = 0.02,
    preAllocateTracks = list(track.height = 0.2),
    target.prop.height = 0.9,
    xmax = m0
  )
  
  circos.trackPlotRegion(
    track.index = 1, 
    bg.border = NA, 
    panel.fun = function(x, y) {
      s <- get.cell.meta.data("sector.index")
      dd <- filter(d0, lab == s)
      xx <- get.cell.meta.data("xlim") %>%
        mean()
      theta <- circlize(mean(xx), 1.3)[1, 1] %% 360
      ff <- ifelse(theta < 90 || theta > 270, 
                   "clockwise", "reverse.clockwise")
      aa <- c(1, 0.5)
      if(theta < 90 || theta > 270)  
        aa <- c(0, 0.5)
      
      flag_disp <- TRUE
      if(is.na(dd$alpha3))
        flag_disp <- FALSE
      if(dd$alpha3 %in% l0$alpha3)
        flag_disp <- FALSE
      
      if(flag_disp){
        flag_rot <- ifelse(theta < 90 || theta > 270, -90, 90)
        flag <- dd$alpha3 %>%
          paste0("./flags/", . ,".svg") %>%
          image_read() %>%
          image_rotate(degrees = flag_rot)
        circos.raster(image = flag, x = mean(xx), y = 0.2, 
                      width = "0.3cm", facing = "inside")
        
      }
      circos.text(x = xx, y = ifelse(flag_disp, 0.38, 0.1), 
                  labels = dd$label, facing = ff, adj = aa,
                  col = "white", cex = 0.8)
    }
  )
  y <- d4$year[1]
  if(y %in% x4$year){
    logo <- x4 %>%
      filter(year == y) %>%
      slice(1) %>%
      pull(url_comp_logo) %>%
      paste0("https:", .) %>%
      image_read()
    # %>%
      # image_scale("0")
    w <- h <- NULL
    if(y != 2000)
      w <- 0.1
    if(y == 2000)
      h <- 0.15
    grid.raster(image = logo, x = 0.99, y = 0.99, 
                width = w, height = h, 
                hjust = 1, vjust = 1)
  }
  
  if(y %% 4 != 0)
    y <- NULL
  text(-1.1,1.02, paste0("Euro Squads ", y), col="white", cex=1.4, pos=4)
  text(-1.1,0.96,"Leagues to National Teams", col="white", cex=1, pos=4)
  text(-1.1,0.90,"By @guyabelguyabel", col="white", cex=0.7, pos=4)
  
  
  text(1.1,-0.9,"Details:", col="white", cex=0.7, pos=2)
  yy = -0.94
  # text(1.1,x,                              "", col="white", cex=0.5, pos=2)
  # text(1.1,x-0.03,                 "", col="white", cex=0.5, pos=2)
  text(1.1,yy-0.03*0,                      "Colours based on the shirt of each team.", col="white", cex=0.5, pos=2)
  text(1.1,yy-0.03*1,                "Chords represent the connections between the", col="white", cex=0.5, pos=2)
  text(1.1,yy-0.03*2,            "league system of a player’s club (at the lines base)", col="white", cex=0.5, pos=2)
  text(1.1,yy-0.03*3,    "and their national team (at the arrow head). Chord thickness", col="white", cex=0.5, pos=2)
  text(1.1,yy-0.03*4,"represent the number of players per league-national team combination.", col="white", cex=0.5, pos=2)

  if(f %% 10 == 0)
    message(f)
}
dev.off()
file.show("./plot/temp.pdf")




##
## animation
##
# set up frames for animation, pauses on first and last plot
ff <- c(rep(1, 5), 2:50, rep(51, 5))

# bring in image files from each page of PDF
pp <- image_read_pdf("./plot-ims2020/zone_fixed_time.pdf")
# image_info(pp)

# create animated version of images in the PDF
saveVideo(expr = {
  for(j in ff){
    img1 <- pp[j]
    par(mar = rep(0,4))
    plot(as.raster(img1))
  }}, 
  ani.width = 2100, ani.height = 2100, n = length(ff), 
  loop = TRUE, interval = 1/10,
  ffmpeg = "C:/ffmpeg/bin/ffmpeg.exe",
  video.name = "./plot-ims2020/zone_fixed_time.mp4"
)
file.show("./plot-ims2020/zone_fixed_time.mp4")


##
## placeholder image
##
i = "zone_fixed_time"
p <- image_read_pdf(paste0("./plot-ims2020/",i,".pdf"), pages = 1)
png(filename = paste0("./plot-ims2020/",i,".png"), width = 2100, height = 2100)
par(mar = rep(0,4))
plot(as.raster(p))
dev.off()