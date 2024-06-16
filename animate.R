library(tidyverse)
library(animation)
library(magick)
library(pdftools)

# n <- pdf_info("./plot/euro_flex_chord.pdf")$pages
# n <- pdf_info("./plot/euro_fix_chord.pdf")$pages
n <- pdf_info("./plot/euro_fix_chord_v24.pdf")$pages
# reading every page into one object was really buggy
# p <- image_read_pdf("./plot/euro_chord_flex.pdf")
w3 <- read_csv("./data/wiki_comp.csv")

# data frame for animation
d <- tibble(
  page = 1:n
) %>%
  mutate(year = seq(1960, 2024, length.out = n()),
         comp = year %% 4 == 0, 
         comp_last = year == max(year)) %>%
  group_by(page) %>%
  mutate(page_rep = paste(rep(page, 20), collapse = ","),
         page_last = paste(rep(page, 50), collapse = ",")) %>%
  ungroup() %>%
  mutate(f = case_when(
    comp_last ~ page_last, 
    comp ~ page_rep, 
    TRUE ~ as.character(page)
  ))

# frames for animation
ff <- d %>%
  pull(f) %>%
  paste0(collapse = ",") %>%
  str_split(pattern = ",") %>%
  .[[1]] %>%
  as.numeric()

# create animated version of images in the PDF
saveVideo(expr = {
  for(i in ff){
    # p <- image_read_pdf(path = "plot/euro_flex_chord.pdf", pages = i)
    # p <- image_read_pdf(path = "plot/euro_fix_chord.pdf", pages = i)
    p <- image_read_pdf(path = "plot/euro_fix_chord_v24.pdf", pages = i)
    
    if(i %in% d$page[d$comp]){
      logo <- paste0("./logo/", d$year[i], ".png") %>%
        image_read(density = 300) %>%
        image_resize(geometry = "350x350") %>%
        image_extent(geometry = "350x350", gravity = "north-east") %>%
        image_border(color = "grey40")
      
      p <- p %>%
        image_composite(logo, gravity = "north-east")
    }
    
    y <- d$year[i] - d$year[i] %% 4
    p <- p %>%
      image_annotate(
        text = paste0("Euro ", y, " Squads"), 
        color = "white", size = 18, location = "+10+10"
      ) %>%
      image_annotate(
        text = "Where do national teams players\nlive? by @guyabelguyabel",
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
      )
    message(i)
    par(mar = rep(0,4))
    plot(as.raster(p))
  }}, 
  ani.width = 2100, ani.height = 2100, n = length(ff), 
  loop = TRUE, interval = 1/10,
  ffmpeg = "C:/ffmpeg/bin/ffmpeg.exe",
  # video.name = "./plot/euro_flex.mp4"
  # video.name = "./plot/euro_fix.mp4"
  video.name = "./plot/euro_fix_v24.mp4"
)
# file.show("./plot/euro_flex.mp4")
# file.show("./plot/euro_fix.mp4")
file.show("./plot/euro_fix_v24.mp4")

# speed up the video...
# https://online-video-cutter.com/change-video-speed# 