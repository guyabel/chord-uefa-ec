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
w1 <- read_csv("./data/wiki_players.csv")
w2 <- read_csv("./data/wiki_colours.csv")
w3 <- read_csv("./data/wiki_comp.csv")

# club_country to nat_team data frame
d1 <- w1 %>%
  select(year, nat_team, club_country2, contains("alpha3")) %>%
  rename(club_country = club_country2,
         nat_alpha3 = nat_team_alpha3) %>%
  replace_na(list(club_country = "No Club")) %>%
  group_by(club_country, nat_team, year, club_alpha3, nat_alpha3) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  left_join(w2, by = c("nat_alpha3" = "team_alpha3")) %>%
  select(-url_team) %>%
  arrange(year)

n0 <- d1 %>%
  select(contains("nat_"), contains("kit")) %>%
  distinct() %>%
  mutate(nat_team = factor(x = nat_team, levels = unique(d1$nat_team))) %>%
  arrange(nat_team) %>%
  mutate(nat_team = as.character(nat_team)) %>%
  rename(lab = nat_team, 
         alpha3 = nat_alpha3)

c0 <- d1 %>%
  select(contains("club_")) %>%
  distinct() %>%
  filter(!(club_country %in% n0$lab)) %>%
  mutate(kit_shirt = "transparent", 
         kit_shorts = "transparent", 
         kit_socks = "transparent",
         club_country = factor(x = club_country, levels = unique(d1$club_country)),
         club_country = fct_rev(club_country)) %>%
  arrange(club_country) %>%
  mutate(club_country = as.character(club_country)) %>%
  rename(lab = club_country,
         alpha3 = club_alpha3)


d0 <- n0 %>%
  bind_rows(c0) %>%
  mutate(label = str_wrap(string = lab, width = 10)) %>%
  separate(col = label, into = c("lab1", "lab2"), sep = "\n", 
           fill = "right", remove = FALSE) %>%
  mutate(y = ifelse(test = !is.na(lab2), yes = 1, no = 0.6),
         label = ifelse(test = alpha3 %in% c("DEU", "GB-NIR", "IRL", "CZE"),
                        label, lab),
         label = ifelse(label == "Czechoslovakia", "Czecho-\nslovakia", label),
         label = ifelse(label == "Bosnia and Herzegovina", "Bosnia & Herzegovina", label))
                  

z <- expand_grid(year = unique(sort(d1$year)),
                 nat_team = d0$lab,
                 club_country = d0$lab) %>%
  mutate(n = ifelse(nat_team == club_country, 0.01, 0),
         kit_shirt = "transparent", 
         kit_shorts = "transparent", 
         kit_socks = "transparent",
         kit_away = "transparent")

d2 <- d1 %>%
  bind_rows(z) %>%
  group_by(year, nat_team, club_country) %>%
  filter(n == max(n)) %>%
  ungroup() %>%
  arrange(year) %>%
  select(-contains("alpha3"))


m0 <- d2 %>%
  # mutate(n = ifelse(n == 0.1, 0, n)) %>%
  group_by(year) %>%
  complete(club_country, nat_team, fill = list(n = 0)) %>%
  sum_turnover(orig_col = "club_country", dest_col = "nat_team",
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
  mutate(corridor = paste(club_country, nat_team, sep = " -> ")) %>%
  select(-club_country, -nat_team, -contains("kit"), kit_shirt) %>%
  mutate(ease = "linear") %>%
  tween_elements(time = "year", group = "corridor", ease = "ease", 
                 nframes = diff(range(d1$year)) * 4) %>%
  as_tibble() %>%
  separate(col = .group, into = c("club_country", "nat_team"), sep = " -> ") %>%
  relocate(club_country, nat_team, n)





pdf(file = "./plot/temp.pdf", height = 7, width = 7, useDingbats = FALSE)
for(f in unique(d3$.frame)){
# for(f in max(d3$.frame)){
# for(f in min(d3$.frame)){
  d4 <- d3 %>%
    filter(.frame == f)
    # filter(year ==  1976)
    # filter(year ==  1976,
    #        kit_shirt != "#FEFFFE00")
  if(!d4$year[1] %in% w3$year)
    next()
  
  par(mar = rep(0, 4), bg = "grey40", lheight = 0.8)
  circos.clear()
  circos.par(track.margin=c(0.01, -0.01), points.overflow.warning = FALSE,
             start.degree = 90, gap.degree = 3)
  
  # plot the chord diagram
  chordDiagram(
    x = select(d4, nat_team, club_country, n),
    col = pull(d4, kit_shirt),
    # link.border = pull(d4, kit_shorts),
    # link.border = pull(d4, kit_away),
    order = d0 %>%
      filter(lab %in% unique(c(d4$nat_team, d4$club_country))) %>%
      pull(lab),
    grid.col = d0 %>%
      select(lab, kit_shirt) %>%
      deframe(),
    transparency = 0.1,
    directional = -1, direction.type = c("diffHeight", "arrows"),
    link.arr.type = "big.arrow", diffHeight  = -0.02,
    link.sort = TRUE,
    link.largest.ontop = TRUE,
    h.ratio = 0.6,
    xmax = m0,
    annotationTrack = "grid",
    annotationTrackHeight = 0.02,
    preAllocateTracks = list(track.height = 0.2),
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
      if(dd$alpha3 %in% c0$alpha3)
        flag_disp <- FALSE
      
      if(flag_disp){
        flag_rot <- ifelse(theta < 90 || theta > 270, -90, 90)
        flag <-
          dd$alpha3 %>%
          # paste0("./flags/", . ,".svg") %>%
          paste0("./flags/", . ,".png") %>%
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
  if(y %in% w3$year){
    logo <- w3 %>%
      filter(year == y) %>%
      slice(1) %>%
      pull(url_comp_logo) %>%
      paste0("https:", .) %>%
      image_read()
    # %>%
      # image_scale("0")
    w <- h <- NULL
    if(y != 2000)
      w <- 0.14
    if(y == 2000)
      h <- 0.18
    grid.raster(image = logo, x = 0.99, y = 0.99, 
                width = w, height = h, 
                hjust = 1, vjust = 1)
  }
  
  if(y %% 4 != 0)
    y <- NULL
  text(-1.1,1.02, paste0("Euro Squads ", y), col="white", cex=1.4, pos=4)
  text(-1.1,0.96,"Club countries to national teams", col="white", cex=0.75, pos=4)
  text(-1.1,0.91,"by @guyabelguyabel", col="white", cex=0.75, pos=4)
  
  
  text(-1.1,-0.75,"Details:", col="white", cex=0.7, pos=4)
  yy = -0.79
  # text(1.1,x,                              "", col="white", cex=0.5, pos=2)
  # text(1.1,x-0.03,                 "", col="white", cex=0.5, pos=2)
  
  text(-1.1,yy-0.03*0, "Colours based on the", col="white", cex=0.5, pos=4)
  text(-1.1,yy-0.03*1, "shirt of each national team.", col="white", cex=0.5, pos=4)
  text(-1.1,yy-0.03*2, "Chords represent the connections", col="white", cex=0.5, pos=4)
  text(-1.1,yy-0.03*3, "between the country of a player’s club", col="white", cex=0.5, pos=4)
  text(-1.1,yy-0.03*4, "(at the chord base) and their national", col="white", cex=0.5, pos=4)
  text(-1.1,yy-0.03*5, "team (at the arrow head). Chord thickness", col="white", cex=0.5, pos=4)
  text(-1.1,yy-0.03*6, "represents the number of players per club", col="white", cex=0.5, pos=4)
  text(-1.1,yy-0.03*7, "country-national team combination. Created", col="white", cex=0.5, pos=4)
  text(-1.1,yy-0.03*8, "in R. Data scraped from squad lists on Wikipedia:", col="white", cex=0.5, pos=4)
  text(-1.1,yy-0.03*9, "https://en.wikipedia.org/wiki/UEFA_European_Championship", col="white", cex=0.5, pos=4)
  

  if(f %% 10 == 0)
    message(f)
}
dev.off()
file.show("./plot/temp.pdf")




##
## animation
##
# set up frames for animation, pauses on first and last plot
f0 <- d3 %>%
  select(.frame, year) %>%
  distinct() %>%
  mutate(page = 1:n(),
         comp = year %% 4 == 0) %>%
  # rowwise() %>%
  group_by(page) %>%
  mutate(page_rep = paste(rep(page, 20), collapse = ",")) %>%
  ungroup() %>%
  mutate(f = ifelse(comp, page_rep, page))

ff <- f0 %>%
  pull(f) %>%
  paste0(collapse = ",") %>%
  str_split(pattern = ",") %>%
  .[[1]] %>%
  as.numeric()
  
# bring in image files from each page of PDF
pp <- image_read_pdf("./plot/temp.pdf")
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
  video.name = "./plot/temp.mp4"
)
file.show("./plot/temp.mp4")


##
## placeholder image
##
i = "zone_fixed_time"
p <- image_read_pdf(paste0("./plot-ims2020/",i,".pdf"), pages = 1)
png(filename = paste0("./plot-ims2020/",i,".png"), width = 2100, height = 2100)
par(mar = rep(0,4))
plot(as.raster(p))
dev.off()