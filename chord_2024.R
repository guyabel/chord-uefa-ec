library(tidyverse)
library(tweenr)
library(circlize)
library(migest)
library(magick)
library(countrycode)

##
## wikipedia data
##
w1 <- read_csv("./data/wiki_players.csv")
w2 <- read_csv("./data/wiki_colours.csv")
w3 <- read_csv("./data/wiki_comp.csv")

# club_country to nat_team data frame
d <- w1 %>%
  filter(year == 2024) %>%
  select(year, nat_team, club_country_harm, contains("alpha3")) %>%
  rename(club_country = club_country_harm,
         nat_alpha3 = nat_team_alpha3) %>%
  replace_na(list(club_country = "No Club")) %>%
  mutate(nat_team = fct_inorder(nat_team)) %>%
  group_by(club_country, nat_team, year, club_alpha3, nat_alpha3) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(nat_team)

# nation codes and kit colours
n0 <- d %>%
  select(contains("nat_")) %>%
  distinct() %>%
  left_join(w2, by = c("nat_alpha3" = "team_alpha3")) %>%
  mutate(nat_team = as.character(nat_team)) %>%
  rename(lab = nat_team, 
         alpha3 = nat_alpha3)

# nations of players clubs not in tournament
c0 <- d %>%
  select(contains("club_")) %>%
  distinct() %>%
  filter(!(club_country %in% n0$lab)) %>%
  mutate(c1 = "transparent", 
         kit_shorts = "transparent", 
         kit_socks = "transparent",
         club_country = fct_inorder(club_country),
         club_country = fct_rev(club_country)) %>%
  arrange(club_country) %>%
  mutate(club_country = as.character(club_country),
         region = countrycode(sourcevar = club_alpha3, origin = "iso3c",
                              destination = "region"),
         region = fct_relevel(region, "Europe & Central Asia"),
         region = fct_rev(region)
         ) %>%
  arrange(region) %>%
  rename(lab = club_country,
         alpha3 = club_alpha3) 
  

# label details
r <- n0 %>%
  bind_rows(c0) %>%
  mutate(gap = ifelse(c1 == "transparent", 2.5, 1.5),
         label = case_when(
           lab == "Bosnia and Herzegovina" ~ "Bosnia",
           lab == "North Macedonia" ~ "North\nMacedonia", 
           lab == "Czech Republic" ~ "Czech\nRepublic",
           lab == "United Arab Emirates" ~ "UAE",
           TRUE ~ lab
         ))
                  

# plot into pdf
pdf(file = "./plot/euro_2024_chord.pdf", useDingbats = FALSE)

par(mar = rep(0, 4), bg = "grey40", lheight = 0.8)
circos.clear()
circos.par(track.margin = c(0.01, -0.01), 
           points.overflow.warning = FALSE,
           gap.degree = 2.5,
           start.degree = 90)

# plot the chord diagram
chordDiagram(
  x = select(d, nat_team, club_country, n),
  order = r$lab,
  grid.col = r %>%
    select(lab, c1) %>%
    deframe(),
  transparency = 0.1,
  directional = -1, 
  direction.type = c("diffHeight", "arrows"),
  link.arr.type = "big.arrow", 
  diffHeight  = -0.02,
  link.sort = TRUE,
  link.largest.ontop = TRUE,
  h.ratio = 0.6,
  annotationTrack = "grid",
  annotationTrackHeight = 0.02,
  preAllocateTracks = list(track.height = 0.2),
)

circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    s <- get.cell.meta.data("sector.index")
    rr <- filter(r, lab == s)
    xx <- get.cell.meta.data("xlim") %>%
      mean()

    flag_disp <- TRUE
    if(is.na(rr$alpha3))
      flag_disp <- FALSE
    if(rr$alpha3 %in% c0$alpha3)
      flag_disp <- FALSE
    
    if(flag_disp){
      flag <- rr$alpha3 %>%
        paste0("./flag/", . ,".png") %>%
        image_read()
      circos.raster(image = flag, x = mean(xx), y = 0.2, 
                    width = "0.4cm", facing = "clockwise", niceFacing = TRUE)
      
    }
    circos.text(x = xx, y = ifelse(flag_disp, 0.38, 0.1), 
                labels = rr$label, adj = c(0, 0.5),
                facing = "clockwise", niceFacing = TRUE,
                col = "white", cex = 0.8)
  }
)
dev.off()
file.show("./plot/euro_2024_chord.pdf")

library(magick)
p <- image_read_pdf("./plot/euro_2024_chord.pdf")
# pp <- image_read_pdf("./plot/euro_2020.pdf")
# pp

logo <- paste0("./logo/", 2024, ".png") %>%
  image_read(density = 300) %>%
  image_resize(geometry = "350x350") %>%
  image_extent(geometry = "350x350", gravity = "north-east") %>%
  image_border(color = "grey40")

# x1 <- 
p %>%
  image_composite(logo, gravity = "north-east") %>%
  image_annotate(
    text = "Euro 2024 Squads", 
    color = "white", size = 18, location = "+10+10"
  ) %>%
  image_annotate(
    text = "Leagues to national teams\nby @guyabelguyabel",
    color = "white", size = 10, location = "+10+100"
  ) %>%
  image_annotate(
    color = "white", size = 10, gravity = "south-west", location = "+10+330",
    text = "Details:"
  ) %>%
  image_annotate(
    color = "white", size = 6, gravity = "south-west", location = "+10+10",
    text = "Colours based on the
shirt of each national
team. Chords represent
connections between the
country of a player’s club (at
the chord base) and their national
team (at the arrow head). Chord thickness
represents the number of players per club
country-national team combination. Created
in R. Data scraped from squad lists on Wikipedia:
https://en.wikipedia.org/wiki/UEFA_European_Championship"
  ) %>%
  image_write(path = "./plot/euro_2024.png")

file.show("./plot/euro_2024.png")
# p1
