library(tidyverse)
library(tweenr)
library(circlize)
library(migest)
library(magick)

##
## wikipedia data
##
w1 <- read_csv("./data/wiki_players.csv")
w2 <- read_csv("./data/wiki_colours.csv")
w3 <- read_csv("./data/wiki_comp.csv")

# club_country to nat_team data frame
d <- w1 %>%
  left_join(w2, by = c("nat_team_alpha3" = "team_alpha3")) %>%
  select(year, nat_team, club_country_harm, kit_shirt, contains("alpha3")) %>%
  rename(club_country = club_country_harm,
         nat_alpha3 = nat_team_alpha3) %>%
  replace_na(list(club_country = "No Club")) %>%
  mutate(
    nat_team = case_when(
      nat_team == "Yugoslavia" ~ "Serbia",
      nat_team == "FR Yugoslavia"~ "Serbia",
      nat_team == "Serbia & Montenegro" ~ "Serbia",
      nat_team == "West Germany" ~ "Germany",
      nat_team == "USSR" ~ "Russia",
      nat_team == "CIS" ~ "Russia",
      nat_team == "Czechoslovakia" ~ "Czech Republic",
      TRUE ~ nat_team),
    nat_team = fct_inorder(nat_team),
    club_country = case_when(
      club_country == "Yugoslavia" ~ "Serbia",
      club_country == "FR Yugoslavia"~ "Serbia",
      club_country == "Serbia & Montenegro" ~ "Serbia",
      club_country == "West Germany" ~ "Germany",
      club_country == "USSR" ~ "Russia",
      club_country == "CIS" ~ "Russia",
      club_country == "Czechoslovakia" ~ "Czech Republic",
      TRUE ~ club_country)) %>%
  group_by(club_country, nat_team, kit_shirt, year, nat_alpha3, club_alpha3) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  arrange(year, nat_team)

n0 <- d %>%
  distinct(year, nat_team, nat_alpha3, kit_shirt) %>%
  rename(lab = nat_team, 
         alpha3 = nat_alpha3)


c0 <- d %>%
  distinct(club_country, club_alpha3, year) %>%
  filter(!(club_country %in% unique(n0$lab))) %>%
  mutate(kit_shirt = "transparent", 
         club_country = fct_inorder(club_country),
         club_country = fct_rev(club_country)) %>%
  arrange(club_country) %>%
  mutate(club_country = as.character(club_country)) %>%
  rename(alpha3 = club_alpha3, 
         lab = club_country)

# # participating teams
# p <- w3 %>%
#   select(year, team_alpha3) %>%
#   rename(alpha3 = team_alpha3) %>%
#   mutate(alpha3 = ifelse(alpha3 == "CIS", "RUS", alpha3))
# 
# # club countries of teams with players, but the country of club
# # not participating, but has done in previous/future years
# c1 <- d %>%
#   distinct(club_country, club_alpha3, year) %>%
#   filter(club_country %in% unique(n0$lab)) %>%
#   rename(alpha3 = club_alpha3) %>%
#   anti_join(p) %>%
#   rename(lab = club_country) 

b <- c("West Germany", "Northern Ireland", "Republic of Ireland", "Czech Republic") 

r <- n0 %>%
  bind_rows(c0) %>%
  complete(lab, year) %>%
  mutate(
    kit_shirt = ifelse(year >= 2007 & lab == "Serbia", "#DF001C", kit_shirt),
    alpha3 = ifelse(year >= 2007 & lab == "Serbia", "SRB", alpha3)
  ) %>%
  group_by(lab) %>%
  arrange(year) %>%
  fill(alpha3, kit_shirt, .direction = "updown") %>%
  mutate(ease = "linear") %>%
  tween_elements(time = "year", group = "lab", ease = "ease",
                 nframes = diff(range(w1$year)) * 4) %>%
  as_tibble() %>%
  rename(lab = .group) %>%
  mutate(
    lab = as.character(lab),
    label = case_when(
      lab == "Serbia" & year < 1992 ~ "Yugoslavia",
      lab == "Serbia" & year >= 1992 & year < 2003 ~ "FR Yugoslavia",
      lab == "Serbia" & year >= 2003 & year < 2007 ~ "Serbia &\nMontenegro",
      lab == "Germany" & year < 1991 ~ "West Germany",
      lab == "Russia" & year < 1991 ~ "USSR",
      lab == "Russia" & year == 1992 ~ "CIS",
      lab == "Czech Republic" & year < 1993 ~ "Czechoslovakia",
      TRUE ~ lab
    )) %>%
  mutate(
    label = case_when(
      label %in% b ~ str_wrap(string = label, width = 10),
      label == "Czechoslovakia" ~ "Czecho-\nslovakia",
      label == "Bosnia and Herzegovina" ~ "Bosnia and Herz.",
      TRUE ~ label),
    lab = factor(x = lab, levels = c(levels(n0$lab), unique(c0$lab))),
    alpha3 = ifelse(alpha3 == "NA", NA, alpha3)
  ) %>%
  arrange(lab, year)
r

m <- d %>%
  group_by(year) %>%
  rename(orig = club_country, 
         dest = nat_team, 
         flow = n) %>%
  sum_turnover(drop_diagonal = FALSE) %>%
  group_by(region) %>%
  filter(turn == max(turn)) %>%
  slice(1) %>%
  select(region, turn) %>%
  deframe()

z <- expand_grid(year = unique(sort(d$year)),
                 nat_team = unique(r$lab),
                 club_country = unique(r$lab)) %>%
  mutate(n = ifelse(nat_team == club_country, 0.01, 0))
         # kit_shirt = ifelse(nat_team == club_country, NA, "transparent")) 
         
# tween
dd <- d %>%
  select(-contains("alpha3")) %>%
  bind_rows(z) %>%
  group_by(year, nat_team, club_country) %>%
  filter(n == max(n)) %>%
  mutate(corridor = paste(club_country, nat_team, sep = " -> "),
         kit_shirt = ifelse(n == 0.01, "transparent", kit_shirt)) %>%
  group_by(corridor) %>%
  fill(kit_shirt, .direction = "updown") %>%
  arrange(year) %>%
  ungroup() %>%
  select(-club_country, -nat_team) %>%
  relocate(-kit_shirt) %>%
  mutate(ease = "linear") %>%
  tween_elements(time = "year", group = "corridor", ease = "ease", 
                 nframes = diff(range(w1$year)) * 4) %>%
  as_tibble() %>%
  separate(col = .group, into = c("club_country", "nat_team"), sep = " -> ") %>%
  relocate(club_country, nat_team, n) %>%
  mutate(year = round(year, 2))


pdf(file = "./plot/euro_fix_chord.pdf", useDingbats = FALSE)
for(f in unique(dd$.frame)){
# for(f in unique(dd$.frame)[seq(1, 241, 16)]){
  par(mar = rep(0, 4), bg = "grey40", lheight = 0.8)
  circos.clear()
  circos.par(track.margin = c(0.01, -0.01), 
             points.overflow.warning = FALSE,
             gap.degree = 2.5,
             start.degree = 90)
  
  yy <- dd %>%
    filter(.frame == f) %>%
    pull(year) %>%
    unique()
  
  # plot the chord diagram
  chordDiagram(
    x = dd %>%
      filter(.frame == f) %>%
      select(nat_team, club_country, n),
    order = unique(r$lab),
    col = dd %>%
      filter(.frame == f) %>%
      pull(kit_shirt),
    grid.col = r %>%
      filter(.frame == f) %>%
      select(lab, kit_shirt) %>%
      deframe(),
    xmax = m,
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
    preAllocateTracks = list(track.height = 0.25),
  )
  

  circos.trackPlotRegion(
    track.index = 1, 
    bg.border = NA, 
    panel.fun = function(x, y) {
      s <- get.cell.meta.data("sector.index")
      rr <- r %>%
        filter(lab == s, 
               .frame == f)
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
        circos.raster(image = flag, x = mean(xx), y = 0.2 - 0.05, 
                      width = "0.4cm", facing = "clockwise", niceFacing = TRUE)
        
      }
      circos.text(x = xx, y = ifelse(flag_disp, 0.38, 0.1) - 0.08, 
                  labels = rr$label, adj = c(0, 0.5),
                  facing = "clockwise", niceFacing = TRUE,
                  col = "white", cex = 0.8)
    }
  )
  message(f)
}
dev.off()
file.show("plot/euro_fix_chord.pdf")