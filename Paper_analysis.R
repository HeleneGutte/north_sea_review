# Analysis for North Sea review ----

# 1. packages and data ----
library(mapplots)
library(tidyverse)
answers_meta_final <- read_csv("Data/answers_meta_final.csv")

# 2. plotting settings ----
my_colours <- c("climate change" = "orange2", "direct exploitation" = "blue", 
                "invasive species" = "turquoise", "pollution" = "purple", "sea use change" = "gold", 
                "global change" = "darkgreen"
                )

# 3. prepare data table ----

answers_meta_final <- answers_meta_final%>%
  rename("nr" = ...1)%>%
  dplyr::filter(Include == TRUE) # 4739 - 3360 = 1379
# 1379 have been excluded from the analysis

# test how to best split up labels
paper1 <- answers_meta_final%>%
  dplyr::filter(nr == 3 | nr == 4 |  nr == 6 | nr == 7 | nr == 8)%>%
  mutate(methodology = str_split_fixed(Main_methodology, pattern = "\\|\\|\\|", n = Inf), 
         impacts = str_split_fixed(`Analyzed impact(s)`, pattern = "\\|\\|\\|", n = Inf))

# other way: 
paper1 <- answers_meta_final%>%
  dplyr::filter(nr == 3 | nr == 4 |  nr == 6 | nr == 7 | nr == 8)%>%
  #separate_longer_delim(c(`Analyzed impact(s)`, Main_methodology), delim = "\\|\\|\\|")
  separate_rows(`Analyzed impact(s)`, Main_methodology, `Precision_of_the driver(s)`, `ICES_medium_location(s)`, sep = "\\|\\|\\|")
# second way is the preferred one
# I also tried separate_longer_delim, but it did not work, the strings were not separated


# test how to treat other labels
paper1 <- answers_meta_final%>%
  dplyr::filter(!is.na(`Analyzed impact(s) if ""other""`) | !is.na(`Precision of the driver(s) if ""other""`) | !is.na(`Nature of the population if ""other""`))
# 379 have something written in one of the other labels. Some of them should be added to the initial labels e.g. noise and climate change scenario, but some of them are useless or unnecessary. 
# Do it manually?


# 4. Main drivers over time ----
dat <- answers_meta_final%>%
  separate_rows(`Anthropogenic driver(s)`, sep = "\\|\\|\\|")%>%
  select(nr, year, `Anthropogenic driver(s)`)%>%
  filter(year != 2021)%>%
  group_by(year, `Anthropogenic driver(s)`)%>%
  count() %>%
  ungroup()%>%
  add_row(year = 1951, `Anthropogenic driver(s)` = "Direct_exploitation", n = 0, .before = 2)%>%
  add_row(year = 1952, `Anthropogenic driver(s)` = "Direct_exploitation", n = 0, .before = 3)%>%
  add_row(year = 1953, `Anthropogenic driver(s)` = "Direct_exploitation", n = 0, .before = 4)%>%
  add_row(year = 1954, `Anthropogenic driver(s)` = "Direct_exploitation", n = 0, .before = 5)%>%
  add_row(year = 1955, `Anthropogenic driver(s)` = "Direct_exploitation", n = 0, .before = 6)%>%
  add_row(year = 1956, `Anthropogenic driver(s)` = "Direct_exploitation", n = 0, .before = 7)%>%
  add_row(year = 1957, `Anthropogenic driver(s)` = "Direct_exploitation", n = 0, .before = 8)%>%
  add_row(year = 1958, `Anthropogenic driver(s)` = "Direct_exploitation", n = 0, .before = 9)%>%
  add_row(year = 1960, `Anthropogenic driver(s)` = "Direct_exploitation", n = 0, .before = 11)%>%
  add_row(year = 1961, `Anthropogenic driver(s)` = "Direct_exploitation", n = 0, .before = 12)%>%
  add_row(year = 1962, `Anthropogenic driver(s)` = "Direct_exploitation", n = 0, .before = 13)%>%
  add_row(year = 1963, `Anthropogenic driver(s)` = "Direct_exploitation", n = 0, .before = 14)%>%
  add_row(year = 1965, `Anthropogenic driver(s)` = "Direct_exploitation", n = 0, .before = 16)%>%
  add_row(year = 1966, `Anthropogenic driver(s)` = "Direct_exploitation", n = 0, .before = 17)%>%
  add_row(year = 1969, `Anthropogenic driver(s)` = "Direct_exploitation", n = 0, .before = 20)%>%
  pivot_wider(names_from = `Anthropogenic driver(s)`, values_from = n, values_fill = 0)
dat

# absolute numbers
ggplot(dat, aes(x = year))+
  geom_line(aes(y = Direct_exploitation, colour = "direct exploitation"))+
  geom_line(aes(y = Sea_use_change, colour = "sea use change"))+
  geom_line(aes(y = Pollution, colour = "pollution"))+
  geom_line(aes(y = Climate_change, colour = "climate change"))+
  geom_line(aes(y = Biological_invasion, colour = "invasive species"))+
  geom_line(aes(y = Global_change, colour = "global change"))+
  scale_color_manual(name = "Anthropogenic driver", values = c(my_colours))+
  labs(y = "nr of publicatons")

# relative contributions
dat_relative <- dat%>%
  group_by(year)%>%
  mutate("sum_of_papers" = sum(c(Direct_exploitation, Pollution, Sea_use_change, Global_change, Climate_change, Biological_invasion)))%>%
  mutate("de_relative" = Direct_exploitation/sum_of_papers, 
         "p_relative" = Pollution/sum_of_papers,
         "suc_relative" = Sea_use_change/sum_of_papers,
         "cc_relative" = Climate_change/sum_of_papers,
         "bi_relative" = Biological_invasion/sum_of_papers,
         "gc_relative" =Global_change/sum_of_papers)


dat_relative[is.na(dat_relative)] <- 0

ggplot(dat_relative, aes(x = year))+
  geom_line(aes(y = de_relative, colour = "direct exploitation"))+
  geom_line(aes(y = suc_relative, colour = "sea use change"))+
  geom_line(aes(y = p_relative, colour = "pollution"))+
  geom_line(aes(y = cc_relative, colour = "climate change"))+
  geom_line(aes(y = bi_relative, colour = "invasive species"))+
  geom_line(aes(y = gc_relative, colour = "global change"))+
  scale_color_manual(name = "Anthropogenic driver", values = c(my_colours))+
  labs(y = "nr of publicatons", title = "relative contributions")

# Figure 2 spatial distribution of main drivers ----
dat <- answers_meta_final%>%
  filter(year != 2021)%>%  
  select(nr, `Anthropogenic driver(s)`, `ICES_medium_location(s)`)%>%
  separate_longer_delim(`Anthropogenic driver(s)`, delim = "|||")%>%
  separate_longer_delim(`ICES_medium_location(s)`, delim = "|||")%>%
  group_by(`ICES_medium_location(s)`, `Anthropogenic driver(s)`)%>%
  count()

world <- map_data(("world"))
worldmap <- ggplot(world, aes(x = long, y = lat))+
  geom_polygon(mapping = aes(group = group), fill = "white", colour = "black")
NorthSea <- worldmap + coord_cartesian(xlim = c(-4, 12), ylim = c(50, 62))+
  scale_x_continuous(breaks = seq(-4, 12, by = 2))+
  scale_y_continuous(breaks = seq(50, 62, by = 1))+
  labs(x = "Longitude", y = "Latitude")
NorthSea  

NorthSea_icesareas <- ggplot() +geom_segment(aes(x = -4, xend = 4, y = 62, yend = 62))+
  geom_segment(aes(x = -4, xend = 4, y = 62, yend = 62))+
  geom_segment(aes(x = -4, xend = -2, y = 59.5, yend = 59.5))+
  geom_segment(aes(x = -2, xend = 0, y = 59, yend = 59))+
  geom_segment(aes(x = 4, xend = 5, y = 58.5, yend = 58.5))+
  geom_segment(aes(x = 2, xend = 6, y = 58, yend = 58))+
  geom_segment(aes(x = 0, xend = 2, y = 57.5, yend = 57.5))+
  geom_segment(aes(x = 6, xend = 8, y = 57.5, yend = 57.5))+
  geom_segment(aes(x = 8, xend = 9, y = 57, yend = 57))+
  geom_segment(aes(x = -4, xend = 0, y = 56, yend = 56))+
  geom_segment(aes(x = 0, xend = 1, y = 55.5, yend = 55.5))+
  geom_segment(aes(x = 3, xend = 9, y = 55.5, yend = 55.5))+
  geom_segment(aes(x = 1, xend = 3, y = 55, yend = 55))+
  geom_segment(aes(x = 1, xend = 3, y = 54, yend = 54))+
  geom_segment(aes(x = 0, xend = 1, y = 53.5, yend = 53.5))+
  geom_segment(aes(x = 2, xend = 3, y = 52, yend = 52))+
  geom_segment(aes(x = 1, xend = 2, y = 51, yend = 51))+
  #now vertical lines
  geom_segment(aes(x = -4, xend = -4, y = 62, yend = 57.5))+
  geom_segment(aes(x = -2, xend = -2, y = 59.5, yend = 59))+
  geom_segment(aes(x = 0, xend = 0, y = 59, yend = 55.5))+
  geom_segment(aes(x = 1, xend = 1, y = 55.5, yend = 55))+
  geom_segment(aes(x = 1, xend = 1, y = 54, yend = 53.5))+
  geom_segment(aes(x = 2, xend = 2, y = 52, yend = 51))+
  geom_segment(aes(x = 2, xend = 2, y = 55, yend = 54))+
  geom_segment(aes(x = 2, xend = 2, y = 58, yend = 57.5))+
  geom_segment(aes(x = 3, xend = 3, y = 54, yend = 52))+
  geom_segment(aes(x = 3, xend = 3, y = 55.5, yend = 55))+
  geom_segment(aes(x = 4, xend = 4, y = 62, yend = 58.5))+
  geom_segment(aes(x = 5, xend = 5, y = 58.5, yend = 55.5))+
  geom_segment(aes(x = 6, xend = 6, y = 58, yend = 57.5))+
  geom_segment(aes(x = 8, xend = 8, y = 58.5, yend = 57))
  
NorthSea_icesareas

my_colours <- c("Climate_change" = "orange2", "Direct_exploitation" = "blue", 
                "Biological_invasion" = "turquoise", "Pollution" = "purple", "Sea_use_change" = "gold", 
                "Global_change" = "darkgreen")

ggplot(data = dat, aes(x = `Anthropogenic driver(s)`, y = n))+
  geom_col(aes(fill = `Anthropogenic driver(s)`))+
  scale_fill_manual(name = "Anthropogenic driver", values = my_colours)+
  facet_wrap(.~`ICES_medium_location(s)`)+
  labs(x = "")+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank())
