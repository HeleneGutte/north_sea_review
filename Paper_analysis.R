# Analysis for North Sea review ----

# 1. packages and data ----
library(mapplots)
library(tidyverse)
answers_meta_final <- read_csv("Data/answers_meta_final.csv")

# 2. plotting settings ----
my_colours <- c("Climate change" = "orange2", "Direct exploitation" = "blue", 
                "Biological invasion" = "turquoise", "Pollution" = "purple", "Sea use change" = "gold", 
                "Global change" = "darkgreen"
                )

# 3. prepare data table ----

answers_meta_final <- answers_meta_final%>%
  rename("nr" = ...1)%>%
  dplyr::filter(Include == TRUE) # 4739 - 3360 = 1379
# 1379 have been excluded from the analysis


# test how to best split up labels
# paper1 <- answers_meta_final%>%
#   dplyr::filter(nr == 3 | nr == 4 |  nr == 6 | nr == 7 | nr == 8)%>%
#   mutate(methodology = str_split_fixed(Main_methodology, pattern = "\\|\\|\\|", n = Inf), 
#          impacts = str_split_fixed(`Analyzed impact(s)`, pattern = "\\|\\|\\|", n = Inf))
# 
# # other way: 
# paper1 <- answers_meta_final%>%
#   dplyr::filter(nr == 3 | nr == 4 |  nr == 6 | nr == 7 | nr == 8)%>%
#   #separate_longer_delim(c(`Analyzed impact(s)`, Main_methodology), delim = "\\|\\|\\|")
#   separate_rows(`Analyzed impact(s)`, Main_methodology, `Precision_of_the driver(s)`, `ICES_medium_location(s)`, sep = "\\|\\|\\|")
# second way is the preferred one
# I also tried separate_longer_delim, but it did not work, the strings were not separated


# test how to treat other labels
paper1 <- answers_meta_final%>%
  dplyr::filter(!is.na(`Analyzed impact(s) if ""other""`) | !is.na(`Precision of the driver(s) if ""other""`) | !is.na(`Nature of the population if ""other""`))
# 379 have something written in one of the other labels. Some of them should be added to the initial labels e.g. noise and climate change scenario, but some of them are useless or unnecessary. 
# Do it manually?
other_impacts <- paper1%>%
  select(`Analyzed impact(s) if ""other""`)%>%
  filter(!(is.na(`Analyzed impact(s) if ""other""`)))%>%
  mutate(`Analyzed impact(s) if ""other""` = tolower(`Analyzed impact(s) if ""other""`))%>%
  group_by(`Analyzed impact(s) if ""other""`)%>%
  count()%>%
  arrange(desc(n))
other_impacts

other_drivers <- paper1%>%
  select(`Precision of the driver(s) if ""other""`)%>%
  filter(!(is.na(`Precision of the driver(s) if ""other""`)))%>%
  mutate(`Precision of the driver(s) if ""other""` = tolower(`Precision of the driver(s) if ""other""`))%>%
  group_by(`Precision of the driver(s) if ""other""`)%>%
  count()%>%
  arrange(desc(n))
other_drivers

other_population <- paper1%>%
  select(`Nature of the population if ""other""`)%>%
  filter(!(is.na(`Nature of the population if ""other""`)))%>%
  mutate(`Nature of the population if ""other""` = tolower(`Nature of the population if ""other""`))%>%
  group_by(`Nature of the population if ""other""`)%>%
  count()%>%
  arrange(desc(n))
other_population

write_csv(paper1, file = "Data/papers_with_other.csv")
write_csv(other_impacts, file = "Data/other_impacts.csv")
write_csv(other_drivers, file = "Data/other_drivers.csv")
write_csv(other_population, file = "Data/other_population.csv")

# 4. Main drivers over time ----
dat <- answers_meta_final%>%
  separate_rows(`Anthropogenic driver(s)`, sep = "\\|\\|\\|")%>%
  mutate(`Anthropogenic driver(s)` = recode(`Anthropogenic driver(s)`, "Biological_invasion" = "Biological invasion",
                                            "Climate_change" = "Climate change", "Direct_exploitation" = "Direct exploitation",
                                            "Global_change" = "Global change", "Sea_use_change" = "Sea use change"))%>%
  select(nr, year, `Anthropogenic driver(s)`)%>%
  filter(year != 2021)%>%
  group_by(year, `Anthropogenic driver(s)`)%>%
  count() %>%
  ungroup()%>%
  add_row(year = 1951, `Anthropogenic driver(s)` = "Direct exploitation", n = 0, .before = 2)%>%
  add_row(year = 1952, `Anthropogenic driver(s)` = "Direct exploitation", n = 0, .before = 3)%>%
  add_row(year = 1953, `Anthropogenic driver(s)` = "Direct exploitation", n = 0, .before = 4)%>%
  add_row(year = 1954, `Anthropogenic driver(s)` = "Direct exploitation", n = 0, .before = 5)%>%
  add_row(year = 1955, `Anthropogenic driver(s)` = "Direct exploitation", n = 0, .before = 6)%>%
  add_row(year = 1956, `Anthropogenic driver(s)` = "Direct exploitation", n = 0, .before = 7)%>%
  add_row(year = 1957, `Anthropogenic driver(s)` = "Direct exploitation", n = 0, .before = 8)%>%
  add_row(year = 1958, `Anthropogenic driver(s)` = "Direct exploitation", n = 0, .before = 9)%>%
  add_row(year = 1960, `Anthropogenic driver(s)` = "Direct exploitation", n = 0, .before = 11)%>%
  add_row(year = 1961, `Anthropogenic driver(s)` = "Direct exploitation", n = 0, .before = 12)%>%
  add_row(year = 1962, `Anthropogenic driver(s)` = "Direct exploitation", n = 0, .before = 13)%>%
  add_row(year = 1963, `Anthropogenic driver(s)` = "Direct exploitation", n = 0, .before = 14)%>%
  add_row(year = 1965, `Anthropogenic driver(s)` = "Direct exploitation", n = 0, .before = 16)%>%
  add_row(year = 1966, `Anthropogenic driver(s)` = "Direct exploitation", n = 0, .before = 17)%>%
  add_row(year = 1969, `Anthropogenic driver(s)` = "Direct exploitation", n = 0, .before = 20)%>%
  pivot_wider(names_from = `Anthropogenic driver(s)`, values_from = n, values_fill = 0)
dat

# absolute numbers
ggplot(dat, aes(x = year))+
  geom_line(aes(y = `Direct exploitation`, colour = "Direct exploitation"))+
  geom_line(aes(y = `Sea use change`, colour = "Sea use change"))+
  geom_line(aes(y = `Pollution`, colour = "Pollution"))+
  geom_line(aes(y = `Climate change`, colour = "Climate change"))+
  geom_line(aes(y = `Biological invasion`, colour = "Biological invasion"))+
  geom_line(aes(y = `Global change`, colour = "Global change"))+
  scale_color_manual(name = "Anthropogenic driver", values = c(my_colours))+
  labs(y = "nr of publicatons")

# relative contributions
dat_relative <- dat%>%
  group_by(year)%>%
  mutate("sum_of_papers" = sum(c(`Direct exploitation`, `Pollution`, `Sea use change`, `Global change`, `Climate change`, `Biological invasion`)))%>%
  mutate("Direct exploitation" = `Direct exploitation`/sum_of_papers, 
         "Pollution" = `Pollution`/sum_of_papers,
         "Sea use change" = `Sea use change`/sum_of_papers,
         "Climate change" = `Climate change`/sum_of_papers,
         "Biological invasion" = `Biological invasion`/sum_of_papers,
         "Global change" = `Global change`/sum_of_papers)


dat_relative[is.na(dat_relative)] <- 0

ggplot(dat_relative, aes(x = year))+
  geom_line(aes(y = `Direct exploitation`, colour = "Direct exploitation"))+
  geom_line(aes(y = `Sea use change`, colour = "Sea use change"))+
  geom_line(aes(y = `Pollution`, colour = "Pollution"))+
  geom_line(aes(y = `Climate change`, colour = "Climate change"))+
  geom_line(aes(y = `Biological invasion`, colour = "Biological invasion"))+
  geom_line(aes(y = `Global change`, colour = "Global change"))+
  scale_color_manual(name = "Anthropogenic driver", values = c(my_colours))+
  labs(y = "nr of publicatons", title = "relative contributions")

# as stacked barplot
dat_relative_2 <- dat_relative%>%
  select(year, `Direct exploitation`:`Biological invasion`)%>%
  pivot_longer(!year, names_to = "Anthropogenic driver", values_to = "Proportion")

ggplot(dat_relative_2, aes(x = year, y = Proportion, fill = `Anthropogenic driver`))+
  geom_col()+
  scale_fill_manual(values = my_colours)+
  labs(x = "Year")

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

# Figure 3 Sankey diagrams for each main driver ----
install.packages("networkD3")
library(networkD3)
dat <- answers_meta_final%>%
  filter(year != 2021)%>%  
  select(nr, `Anthropogenic driver(s)`, `Precision_of_the driver(s)`, `Analyzed impact(s)`, `Nature of the study population`)%>%
  separate_longer_delim(`Anthropogenic driver(s)`, delim = "|||")%>%
  separate_longer_delim(`Precision_of_the driver(s)`, delim = "|||")%>%
  separate_longer_delim(`Analyzed impact(s)`, delim = "|||")%>%
  separate_longer_delim(`Nature of the study population`, delim = "|||")

sankey_ploty <- vector(mode = "list", length = length(unique(dat$`Anthropogenic driver(s)`)))
drivers <- unique(dat$`Anthropogenic driver(s)`)
drivers <- drivers[-7]
for(i in 1:(length(unique(dat$`Anthropogenic driver(s)`))-1)){
  temp <- dat%>%
    filter(`Anthropogenic driver(s)` == drivers[i])%>%
    select(`Precision_of_the driver(s)`, `Analyzed impact(s)`, `Nature of the study population`)%>%
    group_by(`Precision_of_the driver(s)`, `Analyzed impact(s)`, `Nature of the study population`)%>%
    count()
  links <- temp%>%
    transmute(source = `Precision_of_the driver(s)`, target = `Analyzed impact(s)`, value = n, linkgroup = `Analyzed impact(s)`)
  
  links <- rbind(links, temp%>%
                      transmute(source = `Analyzed impact(s)`, target = `Nature of the study population`, value = n, linkgroup= `Analyzed impact(s)`))
  nodes <- data.frame(
    name=c(as.character(links$source), as.character(links$target)) %>% 
      unique())
  nodes$group<-'nodes'
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  sankey_ploty[[i]] <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                     iterations = 0, Value = "value", NodeID = "name",
                     fontSize = 16, fontFamily = 'Ubuntu',
                     #colourScale=my_color, 
                     LinkGroup="linkgroup", NodeGroup = "group")
  
}
sankey_ploty[[1]]
sankey_ploty[[2]]
sankey_ploty[[3]]
sankey_ploty[[4]]
sankey_ploty[[5]]
sankey_ploty[[6]]
