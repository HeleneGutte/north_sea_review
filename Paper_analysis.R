# Analysis for North Sea review ----

# 1. packages and data ----
library(mapplots)
library(RColorBrewer)
library(tidyverse)
library(patchwork)
answers_meta_final <- read_csv("Data/answers_meta_final.csv")

# 2. plotting settings ----
my_colours <- c("Climate change" = "orange2", "Direct exploitation" = "blue", 
                "Biological invasion" = "turquoise", "Pollution" = "purple", "Sea use change" = "gold", 
                "Global change" = "darkgreen"
                )

# 3. prepare data table ----

answers_meta_final <- answers_meta_final%>%
  rename("nr" = ...1)%>%
  dplyr::filter(Include == TRUE)%>%# 4739 - 3360 = 1379
  dplyr::filter(!is.na(`Anthropogenic driver(s)`))%>%# 3360 - 1 = 1380
  dplyr::filter(!is.na(`Precision_of_the driver(s)`))%>%#-2
  dplyr::filter(!is.na(`Analyzed impact(s)`))%>% #-1
  dplyr::filter(!is.na(`Nature of the study population`)) #3356
# 1383 have been excluded from the analysis, 
# because they were marked as irrelevant or missing important information

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

# write_csv(paper1, file = "Data/papers_with_other.csv")
# write_csv(other_impacts, file = "Data/other_impacts.csv")
# write_csv(other_drivers, file = "Data/other_drivers.csv")
# write_csv(other_population, file = "Data/other_population.csv")

# Figure 1 Main drivers over time ----
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
dat_relative_2$`Anthropogenic driver` <- factor(dat_relative_2$`Anthropogenic driver`, levels = rev(c("Pollution", "Direct exploitation", 
                                                                            "Climate change", "Sea use change", 
                                                                            "Biological invasion", "Global change")))

bars_figure1 <- ggplot(dat_relative_2, aes(x = year, y = Proportion, fill = `Anthropogenic driver`))+
  geom_col()+
  scale_fill_manual(values = my_colours, limits = rev(levels(dat_relative_2$`Anthropogenic driver`)))+
  labs(x = "Year")+
  ylim(0, 1)+
  theme_test()+
  theme(legend.position = "bottom")
bars_figure1

nr_papers_figure1 <- ggplot(dat_relative, aes(x = year, y = sum_of_papers))+
  geom_line()+
  labs(y = "Nr of papers", x = "")+
  theme_test()
nr_papers_figure1

gridExtra::grid.arrange(grobs = list(nr_papers_figure1, bars_figure1), nrow = 2, ncol = 1, 
                        heights = c(1, 3))
nr_papers_figure1_grob <- ggplotGrob(nr_papers_figure1)
bars_figure1 + annotation_custom(grob = nr_papers_figure1_grob, 
                                 xmin = 1942, xmax = 2024, ymin = 1, ymax = 1.5)

# Figure 2 spatial distribution of main drivers ----
dat <- answers_meta_final%>%
  filter(year != 2021)%>%  
  select(nr, `Anthropogenic driver(s)`, `ICES_medium_location(s)`)%>%
  separate_longer_delim(`Anthropogenic driver(s)`, delim = "|||")%>%
  separate_longer_delim(`ICES_medium_location(s)`, delim = "|||")%>%
  group_by(`ICES_medium_location(s)`, `Anthropogenic driver(s)`)%>%
  count()%>%
  mutate(`Anthropogenic driver(s)` = recode(`Anthropogenic driver(s)`, "Biological_invasion" = "Biological invasion",
                                                      "Climate_change" = "Climate change", "Direct_exploitation" = "Direct exploitation",
                                                      "Global_change" = "Global change", "Sea_use_change" = "Sea use change"))
  
#with percentages and total numbers
dat <- dat %>% group_by(`ICES_medium_location(s)`) %>%
  mutate (relative_sum_paps = n/sum_paps,
          sum_real = sum (relative_sum_paps))

world <- map_data(("world"))
worldmap <- ggplot(world, aes(x = long, y = lat))+
  geom_polygon(mapping = aes(group = group), fill = "gray90", colour = "black")
NorthSea <- worldmap + coord_cartesian(xlim = c(-4, 12), ylim = c(50, 62))+
  scale_x_continuous(breaks = seq(-4, 12, by = 2))+
  scale_y_continuous(breaks = seq(50, 62, by = 1))+
  labs(x = "Longitude", y = "Latitude")+
  theme_test()
NorthSea  

# NorthSea_icesareas <- ggplot() +geom_segment(aes(x = -4, xend = 4, y = 62, yend = 62))+
#   geom_segment(aes(x = -4, xend = 4, y = 62, yend = 62))+
#   geom_segment(aes(x = -4, xend = -2, y = 59.5, yend = 59.5))+
#   geom_segment(aes(x = -2, xend = 0, y = 59, yend = 59))+
#   geom_segment(aes(x = 4, xend = 5, y = 58.5, yend = 58.5))+
#   geom_segment(aes(x = 2, xend = 6, y = 58, yend = 58))+
#   geom_segment(aes(x = 0, xend = 2, y = 57.5, yend = 57.5))+
#   geom_segment(aes(x = 6, xend = 8, y = 57.5, yend = 57.5))+
#   geom_segment(aes(x = 8, xend = 9, y = 57, yend = 57))+
#   geom_segment(aes(x = -4, xend = 0, y = 56, yend = 56))+
#   geom_segment(aes(x = 0, xend = 1, y = 55.5, yend = 55.5))+
#   geom_segment(aes(x = 3, xend = 9, y = 55.5, yend = 55.5))+
#   geom_segment(aes(x = 1, xend = 3, y = 55, yend = 55))+
#   geom_segment(aes(x = 1, xend = 3, y = 54, yend = 54))+
#   geom_segment(aes(x = 0, xend = 1, y = 53.5, yend = 53.5))+
#   geom_segment(aes(x = 2, xend = 3, y = 52, yend = 52))+
#   geom_segment(aes(x = 1, xend = 2, y = 51, yend = 51))+
#   #now vertical lines
#   geom_segment(aes(x = -4, xend = -4, y = 62, yend = 57.5))+
#   geom_segment(aes(x = -2, xend = -2, y = 59.5, yend = 59))+
#   geom_segment(aes(x = 0, xend = 0, y = 59, yend = 55.5))+
#   geom_segment(aes(x = 1, xend = 1, y = 55.5, yend = 55))+
#   geom_segment(aes(x = 1, xend = 1, y = 54, yend = 53.5))+
#   geom_segment(aes(x = 2, xend = 2, y = 52, yend = 51))+
#   geom_segment(aes(x = 2, xend = 2, y = 55, yend = 54))+
#   geom_segment(aes(x = 2, xend = 2, y = 58, yend = 57.5))+
#   geom_segment(aes(x = 3, xend = 3, y = 54, yend = 52))+
#   geom_segment(aes(x = 3, xend = 3, y = 55.5, yend = 55))+
#   geom_segment(aes(x = 4, xend = 4, y = 62, yend = 58.5))+
#   geom_segment(aes(x = 5, xend = 5, y = 58.5, yend = 55.5))+
#   geom_segment(aes(x = 6, xend = 6, y = 58, yend = 57.5))+
#   geom_segment(aes(x = 8, xend = 8, y = 58.5, yend = 57))
#   
# NorthSea_icesareas

ggplot(data = dat, aes(x = `Anthropogenic driver(s)`, y = n))+
  geom_col(aes(fill = `Anthropogenic driver(s)`))+
  scale_fill_manual(name = "Anthropogenic driver", values = my_colours)+
  facet_wrap(.~`ICES_medium_location(s)`, scales = "free")+
  scale_y_continuous(limits=c(0, 1000))+
  labs(x = "")+
  theme_test()+
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.position = "bottom", 
        strip.text = element_blank())

# Figure 3 Sankey diagrams for each main driver ----
#install.packages("networkD3")
library(networkD3)
dat <- answers_meta_final%>%
  filter(year != 2021)%>%  
  select(nr, `Anthropogenic driver(s)`, `Precision_of_the driver(s)`, `Analyzed impact(s)`, `Nature of the study population`)%>%
  separate_longer_delim(`Anthropogenic driver(s)`, delim = "|||")%>%
  separate_longer_delim(`Precision_of_the driver(s)`, delim = "|||")%>%
  separate_longer_delim(`Analyzed impact(s)`, delim = "|||")%>%
  separate_longer_delim(`Nature of the study population`, delim = "|||")

dat[dat$`Precision_of_the driver(s)` == "Other", ]$`Precision_of_the driver(s)` <- "Other_driver"  
dat[dat$`Analyzed impact(s)` == "Other", ]$`Analyzed impact(s)` <- "Other_impact"
dat[dat$`Nature of the study population` == "Other", ]$`Nature of the study population` <- "Other_population"

unique(dat$`Precision_of_the driver(s)`) #issue that Fishing exploitation has in some case a white space at the beginning
#trim white space at beginning and end
dat <- dat%>%
  mutate(`Anthropogenic driver(s)` = str_trim(`Anthropogenic driver(s)`, "both"), 
         `Precision_of_the driver(s)` = str_trim(`Precision_of_the driver(s)`, "both"),
         `Analyzed impact(s)` = str_trim(`Analyzed impact(s)`, "both"),
         `Nature of the study population` = str_trim(`Nature of the study population`, "both"))
unique(dat$`Precision_of_the driver(s)`)

sankey_ploty <- vector(mode = "list", length = length(unique(dat$`Anthropogenic driver(s)`)))
drivers <- unique(dat$`Anthropogenic driver(s)`)

n <- length(unique(dat$`Nature of the study population`))
pal <- unlist(mapply(brewer.pal,9,'Set1'))
pal2 <- unlist(mapply(brewer.pal,5,'Set2'))
pal3 <- unlist(mapply(brewer.pal,6,'Set3'))
pal<-rbind(pal, pal2)
pal<-paste(shQuote(pal), collapse=", ")
dom<-unique(unique(dat$`Nature of the study population`))
dom<-paste(shQuote(dom), collapse=", ")
my_color <-paste0("d3.scaleOrdinal().domain([",dom,",'nodes']).range([",pal,",'grey'])")



for(i in 1:(length(unique(dat$`Anthropogenic driver(s)`)))){
  temp <- dat%>%
    filter(`Anthropogenic driver(s)` == drivers[i])%>%
    select(`Precision_of_the driver(s)`, `Analyzed impact(s)`, `Nature of the study population`)%>%
    group_by(`Precision_of_the driver(s)`, `Analyzed impact(s)`, `Nature of the study population`)%>%
    count()
  links <- temp%>%
    transmute(source = `Precision_of_the driver(s)`, target = `Analyzed impact(s)`, value = n, linkgroup = `Analyzed impact(s)`)
  
  links <- rbind(links, temp%>%
                      transmute(source = `Analyzed impact(s)`, target = `Nature of the study population`, value = n, linkgroup= `Analyzed impact(s)`))
  
  #sort links table by count of analyzed impact
  links_sort <- links%>%
    ungroup()%>%
    select(`Analyzed impact(s)`, value)%>%
    group_by(`Analyzed impact(s)`)%>%
    mutate(nr = sum(value))%>%
    select(`Analyzed impact(s)`, nr)%>%
    distinct()%>%
    arrange(desc(nr))
  
  links_ordered <- match(links$`Analyzed impact(s)`, links_sort$`Analyzed impact(s)`)
  links_new <- links[order(links_ordered), ]
    
  nodes <- data.frame(
    name=c(as.character(links_new$source), as.character(links_new$target)) %>% 
      unique())
  nodes$group<-'nodes'
  links_new$IDsource <- match(links_new$source, nodes$name)-1 
  links_new$IDtarget <- match(links_new$target, nodes$name)-1
  sankey_ploty[[i]] <- sankeyNetwork(Links = links_new, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                     iterations = 0, Value = "value", NodeID = "name",
                     fontSize = 16, fontFamily = 'Ubuntu',
                     colourScale = my_color, 
                     LinkGroup= "Nature of the study population", NodeGroup = "group")
  
}
sankey_ploty[[1]]
sankey_ploty[[2]]
sankey_ploty[[3]]
sankey_ploty[[4]]
sankey_ploty[[5]]
sankey_ploty[[6]]

#Figure 4 Driver in nr of papers----
dat <- answers_meta_final%>%
filter(year != 2021)%>%  
select(year,`Anthropogenic driver(s)`, `Organizational level`)%>%
separate_longer_delim(`Anthropogenic driver(s)`, delim = "|||")%>%
separate_longer_delim(`Organizational level`, delim = "|||")%>% 
  mutate(`Anthropogenic driver(s)` = recode(`Anthropogenic driver(s)`, "Biological_invasion" = "Biological invasion",
                                            "Climate_change" = "Climate change", "Direct_exploitation" = "Direct exploitation",
                                            "Global_change" = "Global change", "Sea_use_change" = "Sea use change")) %>% 
  mutate(`Organizational level` = recode(`Organizational level`, "Community" = "Community", "Ecosystem" = "Ecosystem",
                                         "Individual" = "Individual", "Not_applicable" = "Not applicable", "Population"= "Population")) %>% 
group_by(year,`Anthropogenic driver(s)`,`Organizational level`)%>%
count() %>% 
ungroup() %>%
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
drop_na() 


  
#order:
# 1. Pollution
# 2. Direct exploitation
# 3. Climate change
# 4. Sea use chage
# 5. Biological invasion
# 6. Global change

#add colors
#1. Pollution
dat_1 <- dat[dat$`Anthropogenic driver(s)` == "Pollution",]

colours <- NULL
colours[dat_1$`Organizational level` == "Ecosystem"]      <-"mediumorchid1"
colours[dat_1$`Organizational level` == "Community"]      <-"mediumorchid"
colours[dat_1$`Organizational level` == "Population"]     <-"purple"
colours[dat_1$`Organizational level` == "Individual"]     <-"purple3"
colours[dat_1$`Organizational level` == "Not applicable"] <-"purple4"
dat_1$colours <- colours

#2. Direct exploitation
dat_2 <- dat[dat$`Anthropogenic driver(s)` == "Direct exploitation",]

colours <- NULL
colours[dat_2$`Organizational level` == "Ecosystem"]      <-"lightblue2"
colours[dat_2$`Organizational level` == "Community"]      <-"deepskyblue1"
colours[dat_2$`Organizational level` == "Population"]     <-"dodgerblue2"
colours[dat_2$`Organizational level` == "Individual"]     <-"deepskyblue3"
colours[dat_2$`Organizational level` == "Not applicable"] <-"dodgerblue4"
dat_2$colours <- colours

#3. Climate change
dat_3 <- dat[dat$`Anthropogenic driver(s)` == "Climate change",]

colours <- NULL
colours[dat_3$`Organizational level` == "Ecosystem"]      <-"orange"
colours[dat_3$`Organizational level` == "Community"]      <-"orange1"
colours[dat_3$`Organizational level` == "Population"]     <-"orange2"
colours[dat_3$`Organizational level` == "Individual"]     <-"orange3"
colours[dat_3$`Organizational level` == "Not applicable"] <-"orange4"
dat_3$colours <- colours

#4. Sea use change
dat_4 <- dat[dat$`Anthropogenic driver(s)` == "Sea use change",]

colours <- NULL
colours[dat_4$`Organizational level` == "Ecosystem"]      <-"gold"
colours[dat_4$`Organizational level` == "Community"]      <-"gold1"
colours[dat_4$`Organizational level` == "Population"]     <-"gold2"
colours[dat_4$`Organizational level` == "Individual"]     <-"gold3"
colours[dat_4$`Organizational level` == "Not applicable"] <-"gold4"
dat_4$colours <- colours

#5. Biological invasion
dat_5 <- dat[dat$`Anthropogenic driver(s)` == "Biological invasion",]

colours <- NULL
colours[dat_5$`Organizational level` == "Ecosystem"]      <-"turquoise"
colours[dat_5$`Organizational level` == "Community"]      <-"turquoise1"
colours[dat_5$`Organizational level` == "Population"]     <-"turquoise2"
colours[dat_5$`Organizational level` == "Individual"]     <-"turquoise3"
colours[dat_5$`Organizational level` == "Not applicable"] <-"turquoise4"
dat_5$colours <- colours

#6. Biological invasion
dat_6 <- dat[dat$`Anthropogenic driver(s)` == "Global change",]

colours <- NULL
colours[dat_6$`Organizational level` == "Ecosystem"]      <-"chartreuse"
colours[dat_6$`Organizational level` == "Community"]      <-"darkolivegreen2"
colours[dat_6$`Organizational level` == "Population"]     <-"darkolivegreen3"
colours[dat_6$`Organizational level` == "Individual"]     <-"darkolivegreen4"
colours[dat_6$`Organizational level` == "Not applicable"] <-"darkgreen"
dat_6$colours <- colours

#combine datasets again
dat_all <- rbind(dat_1,dat_2,dat_3,dat_4,dat_5,dat_6)
col <- dat_all$colours

#add levels
dat_all$`Organizational level` <- factor(dat_all$`Organizational level`, levels = c("Ecosystem","Community","Population","Individual","Not_applicable"))

#barplot

barplot <- ggplot(dat_all)+
  geom_bar(aes(x=factor(`Anthropogenic driver(s)`,levels = c("Pollution","Direct exploitation","Climate change",
                                                             "Sea use change","Biological invasion","Global change")),
               y = n, group=`Organizational level`), 
           stat="identity", fill = col)+
  coord_flip()+
  labs(x="Anthropogenic driver", y = "Number of papers")+
  theme_test()+
  theme(legend.position = "bottom")
barplot

#line plots
# 1. Pollution
lines_1 <- ggplot(dat_1)+
  geom_line(mapping= aes(year, n, col = factor(colours)))+
  scale_color_identity()+
  theme_test()+
  labs(x="", y= "")+
  ylim(0,30)+
  xlim(1940,2020)+
  facet_wrap(~factor(`Organizational level`,c("Not applicable","Individual","Population","Community","Ecosystem")),nrow=1)+
  theme(
    strip.background = element_blank(),
    strip.text = element_blank())
lines_1

# 2. Direct exploitation
lines_2 <- ggplot(dat_2)+
  geom_line(mapping= aes(year, n, col = factor(colours)))+
  scale_color_identity()+
  theme_test()+
  labs(x="", y= "")+
  ylim(0,30)+
  xlim(1940,2020)+
  facet_wrap(~factor(`Organizational level`,c("Not applicable","Individual","Population","Community","Ecosystem")),nrow=1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())
lines_2

# 3. Climate change
lines_3 <- ggplot(dat_3)+
  geom_line(mapping= aes(year, n, col = factor(colours)))+
  scale_color_identity()+
  theme_test()+
  labs(x="", y= "")+
  ylim(0,30)+
  xlim(1940,2020)+
  facet_wrap(~factor(`Organizational level`,c("Not applicable","Individual","Population","Community","Ecosystem")),nrow=1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())
lines_3

# 4. Sea use change
lines_4 <- ggplot(dat_4)+
  geom_line(mapping= aes(year, n, col = factor(colours)))+
  scale_color_identity()+
  theme_test()+
  labs(x="", y= "Number of papers")+
  ylim(0,30)+
  xlim(1940,2020)+
  facet_wrap(~factor(`Organizational level`,c("Not applicable","Individual","Population","Community","Ecosystem")),nrow=1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())
lines_4

# 5. Biological invasion
lines_5 <- ggplot(dat_5)+
  geom_line(mapping= aes(year, n, col = factor(colours)))+
  scale_color_identity()+
  theme_test()+
  labs(x="", y= "")+
  ylim(0,30)+
  xlim(1940,2020)+
  facet_wrap(~factor(`Organizational level`,c("Not applicable","Individual","Population","Community","Ecosystem")),nrow=1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank())
lines_5

# 6. Global change
dat_6 [nrow(dat_6) + 1 , ] <- NA

dat_6$`Organizational level` <-  dat_6$`Organizational level` %>% replace_na('Individual')
dat_6$n <-  dat_6$n %>% replace_na(0)

colours <- NULL
colours[dat_6$`Organizational level` == "Ecosystem"]      <-"chartreuse"
colours[dat_6$`Organizational level` == "Community"]      <-"darkolivegreen2"
colours[dat_6$`Organizational level` == "Population"]     <-"darkolivegreen3"
colours[dat_6$`Organizational level` == "Individual"]     <-"darkolivegreen4"
colours[dat_6$`Organizational level` == "Not applicable"] <-"darkgreen"
dat_6$colours <- colours

lines_6 <- ggplot(dat_6)+
  geom_line(mapping= aes(year, n, col = factor(colours)))+
  scale_color_identity()+
  theme_test()+
  labs(x="", y= "")+
  ylim(0,30)+
  xlim(1940,2020)+
  facet_wrap(~factor(`Organizational level`,c("Not applicable","Individual","Population","Community","Ecosystem")),nrow=1)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
lines_6

#combine plots
p1 <- lines_6+lines_5+lines_4+plot_layout(nrow=3)
p1
p2 <- lines_3+lines_2+lines_1+plot_layout(nrow=3)
p2

lines_6+lines_5+lines_4+lines_3+lines_2+lines_1+plot_layout(nrow=6)

#Figure 5 Methods over time ----
dat <-answers_meta_final%>%
  separate_rows(`Main_methodology`, sep = "\\|\\|\\|")%>%
  mutate(`Main_methodology` = recode(`Main_methodology`, "NA"="NA", "Field_observations_measurement"="Field observations measurement",
                                     "Modelling"="Modelling", "Experiment"="Experiment","Review"="Review", "Meta_analysis"="Meta analysis"))%>%
  select(nr, year, `Main_methodology`)%>%
  filter(year != 2021)%>%
  group_by(year, `Main_methodology`)%>%
  count() %>% 
  ungroup() %>% 
  pivot_wider(names_from = `Main_methodology`, values_from = n, values_fill = 0)

ggplot(dat)+
  geom_line(aes(year,n,col = `Main_methodology`))

# relative contributions
dat_relative <- dat%>%
  group_by(year)%>%
  mutate("sum_of_papers" = sum(c(`Field observations measurement`, `Modelling`, `Experiment`, `Review`,`Meta analysis` ,`NA`)))%>%
  mutate("Field observations measurement" = `Field observations measurement`/sum_of_papers, 
         "Modelling" = `Modelling`/sum_of_papers,
         "Experiment" = `Experiment`/sum_of_papers,
         "Review" = `Review`/sum_of_papers,
         "Meta analysis" = `Meta analysis`/sum_of_papers,
         "NA" = `NA`/sum_of_papers) 


dat_relative[is.na(dat_relative)] <- 0


# as stacked barplot
dat_relative_2 <- dat_relative%>%
  select(year, `Field observations measurement`:`Meta analysis`)%>%
  pivot_longer(!year, names_to = "Main_methodology", values_to = "Proportion")
dat_relative_2$`Main_methodology` <- factor(dat_relative_2$`Main_methodology`, levels = rev(c("Field observations measurement", "Experiment","Modelling","Meta analysis","Review","NA")))

#add colors 
meth_colours <- c("Field observations measurement" = "tomato", "Experiment" = "darkseagreen1", 
                "Meta analysis" = "deepskyblue", "Modelling" = "lightsalmon", "Review" = "magenta3", 
                "NA" = "gray25"
)

dat_relative_2$Main_methodology <- factor(dat_relative_2$Main_methodology, levels = c("Field observations measurement","Experiment","Meta analysis","Modelling","Review","NA"))



ggplot(dat_relative_2)+
  geom_bar(aes(x=year,
               y = Proportion, group=`Main_methodology`), 
           stat="identity", fill = colours)+
  labs(x="Anthropogenic driver", y = "Number of papers")+
  theme_test()+
  theme(legend.position = "bottom")

#stacked bar plot
bars_figure5 <- ggplot(dat_relative_2, aes(x = year, y = Proportion, fill = `Main_methodology`))+
  geom_col()+
  scale_fill_manual(values = meth_colours, limits = rev(levels(dat_relative_2$`Main_methodology`)))+
  labs(x = "Year")+
  ylim(0, 1)+
  theme_test()+
  theme(legend.position = "bottom")+ 
  guides(fill=guide_legend(title="Main methodology"))
  
bars_figure5

nr_papers_figure1 <- ggplot(dat_relative, aes(x = year, y = sum_of_papers))+
  geom_line()+
  labs(y = "Nr of papers", x = "")+
  theme_test()
nr_papers_figure1

gridExtra::grid.arrange(grobs = list(nr_papers_figure1, bars_figure5), nrow = 2, ncol = 1, 
                        heights = c(1, 3))

#Figure 6 Methods per driver ----
dat_m <- answers_meta_final%>%
  filter(year != 2021)%>%  
  select(nr, `Anthropogenic driver(s)`,`Main_methodology`)%>%
  separate_longer_delim(`Anthropogenic driver(s)`, delim = "|||")%>%
  separate_longer_delim(`Main_methodology`, delim = "|||") %>% 
  mutate(`Anthropogenic driver(s)` = recode(`Anthropogenic driver(s)`, "Biological_invasion" = "Biological invasion",
                                         "Climate_change" = "Climate change", "Direct_exploitation" = "Direct exploitation",
                                         "Global_change" = "Global change", "Sea_use_change" = "Sea use change")) %>% 
  
  mutate(`Main_methodology` = recode(`Main_methodology`, "NA"="NA", "Field_observations_measurement"="Field observations measurement",
                                     "Modelling"="Modelling", "Experiment"="Experiment","Review"="Review", "Meta_analysis"="Meta analysis"))%>%
  group_by(`Anthropogenic driver(s)`,`Main_methodology` )%>%
  count()


dat_m1 <- dat_m %>% 
  group_by(`Anthropogenic driver(s)`) %>% mutate(sum_p = sum(n))

#add colors
meth_colours <- c("Field observations measurement" = "tomato", "Experiment" = "darkseagreen1", 
                  "Meta analysis" = "deepskyblue", "Modelling" = "lightsalmon", "Review" = "magenta3", 
                  "NA" = "gray25"
)

#stacked bar plot
bars_figure6 <- ggplot(dat_m1, mapping =aes(x = reorder(`Anthropogenic driver(s)`,-sum_p), y = n, fill = `Main_methodology`))+
  geom_col()+
  scale_fill_manual(values = meth_colours)+
  labs(x = "Anthropogenic drivers", y="Number of papers")+
  theme_test()+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title="Main methodology"))
  
bars_figure6

# Figure SI ----
# Datensatz zu groß durch das selecten von allen columns. 
# Daher müsste das mit in die Schleife. 
# dat <- answers_meta_final%>%
#   filter(year != 2021)%>%  
#   select(nr, `Precision_of_the driver(s)`, `Analyzed impact(s)`, 
#          `Nature of the study population`, `ICES_medium_location(s)`)%>%
#   separate_longer_delim(`Precision_of_the driver(s)`, delim = "|||")%>%
#   separate_longer_delim(`Analyzed impact(s)`, delim = "|||")%>%
#   separate_longer_delim(`Nature of the study population`, delim = "|||")%>%
#   separate_longer_delim(`ICES_medium_location(s)`, delim = "|||")
# 
# SI_figures <- vector("list", length = (ncol(dat)-2))
# 
# for(i in 1:(ncol(dat)-2)){
#   x <- sym(names(dat[i+1]))
#   temp <- dat%>%
#     select(nr, !!x, `ICES_medium_location(s)`)%>%
#     mutate(!!x := str_trim(!!x, "both"),
#            `ICES_medium_location(s)` = str_trim(`ICES_medium_location(s)`, "both"))%>%
#     group_by(!!x, `ICES_medium_location(s)`)%>%
#     count()
#   SI_figures[[i]]<- ggplot(temp, aes(x = !!x, y = n, fill = `ICES_medium_location(s)`))+
#     geom_col()+
#     scale_fill_brewer(palette = "Paired")+
#     theme_test()+
#     theme(axis.text.x = element_text(angle = 90, hjust = 1),
#           legend.position = "bottom")+
#     labs(y="Number of papers")
# }
# SI_figures

#1. precision of driver ----
dat_p_d <- answers_meta_final%>%
  filter(year != 2021)%>%  
  select(nr, `Precision_of_the driver(s)`, `ICES_medium_location(s)`, `Analyzed impact(s)`, `Nature of the study population`)%>%
  separate_longer_delim(`Precision_of_the driver(s)`, delim = "|||")%>%
  separate_longer_delim(`ICES_medium_location(s)`, delim = "|||") %>% 
  mutate(`Precision_of_the driver(s)` = recode(`Precision_of_the driver(s)`, 
                                               "Non_native_species_introduction" = "Non native species introduction",
                                               "Chemical_hazardous_substances" = "Chemical hazardous substances", 
                                               "Medical_hormone_products" = "Medical hormone products",
                                               "Nutrient_input_N_P" = "Nutrient input N P",
                                               "Plastic_litter" = "Plastic litter",
                                               "Other_litter" = "Other litter", 
                                               "Other"= "Other",
                                               "Other_physical_parameter" = "Other physical parameter",
                                               "Oils_hydrocarbons_related" ="Oils hydrocarbons related",
                                               "Gas_stockage_leaking" = "Gas stockage leaking",
                                               "Temperature"="Temperature",
                                               "Sea_level_rise"= "Sea level rise",
                                               "Fishing_incidental_or_bycatch" = "Fishing incidental or bycatch",
                                               "Fishing_exploitation" = "Fishing exploitation",
                                               "Metal_substances" = "Metal substances",
                                               " Fishing_exploitation"= "Fishing exploitation",
                                               "Civil_engineering_works"= "Civil engineering works",
                                               "Destructive_fisheries_seabed"= "Destructive fisheries seabed", 
                                               "Radioactive_substances"= "Radioactive substances", 
                                               "Aquaculture_related"= "Aquaculture related",
                                               "Coastal_development" = "Coastal development",
                                               "Collision_shipping"= "Collision shipping",
                                               "Salinity"= "Salinity",
                                               "pH"="pH", 
                                               "Mining"  = "Mining", 
                                               "Global_change"= "Global change",
                                               "Other_fishing"= "Other fishing",
                                               "Oxygen"= "Oxygen",
                                               "Pathogens_from_land_based_sources"= "Pathogens from land based sources",
                                               "Not_specified"="Not specified",
                                               "Fishing_IUU"=    "Fishing IUU" )) %>% 
  mutate(`ICES_medium_location(s)` = recode(`ICES_medium_location(s)`,
                                            "1" ="1", "2"="2","3"="3","4"="4",
                                            "5"="5","6"="6","7"="7","Norvegian_coast" = "Norwegian coast",
                                            "Not_specified" = "Not specified", "Whole_North_Sea" = "Whole North Sea")) %>% 
  group_by(`Precision_of_the driver(s)`,`ICES_medium_location(s)` )%>%
  count()


dat_p_d1 <- dat_p_d %>% 
  group_by(`Precision_of_the driver(s)`) %>% mutate(sum_p = sum(n))

ggplot(dat_p_d1, mapping =aes(x = reorder(`Precision_of_the driver(s)`,-sum_p), y = n, fill = `ICES_medium_location(s)`))+
  geom_col()+
  scale_fill_brewer(palette = "Paired")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "", y = "Number of papers")+
  theme_test() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")+ 
  guides(fill=guide_legend(title="ICES medium locations"))


#2.analyzed impacts ----
dat_a_i <- answers_meta_final%>%
  filter(year != 2021)%>%  
  select(nr, `Analyzed impact(s)`, `ICES_medium_location(s)`)%>%
  separate_longer_delim(`Analyzed impact(s)`, delim = "|||")%>%
  separate_longer_delim(`ICES_medium_location(s)`, delim = "|||") %>% 
  mutate(`Analyzed impact(s)` = recode(`Analyzed impact(s)`, 
                                       "Change_biomass_abundance" ="Change biomass abundance","Change_distribution"="Change distribution",
                                       "Change_chemical_water_properties"="Change chemical water properties", 
                                       "Quantification_driver"= "Quantification driver","Change_physical_water_properties"  = "Change physical water properties",  "Other"="Other",                          
                                       "Change_diversity"="Change diversity","Change_genetic"="Change diversity",   "Change_trophic_functioning"="Change trophic functioning",   
                                       "Change_demographic_structure"="Change demographic structure","Change_chemical_sediment_properties" ="Change chemical sediment properties", "Degradation_habitat_loss"="Degradation habitat loss",    
                                       "Change_extreme_events"="Change extreme events","Change_life_history_traits"="Change life history traits", "Change_contamination_level"="Change contamination level",
                                       "Coastal_erosion_sedimentation"="Coastal erosion sedimentation", "Change_physical_sediment_properties" = "Change physical sediment properties","Multiple"="Multiple",        
                                       "Plastic_ingestion"="Plastic ingestion","Change_physiology"="Change physiology", "Bioaccumulation_TL"="Bioaccumulation TL",           
                                       "Change_phenology"="Change phenology","Change_biogeochemical_fluxes"="Change biogeochemical fluxes", "Incidental_mortality"="Incidental mortality",      
                                       "Change_Harmful_algal_blooms"="Change Harmful algal blooms","Change_toxins" ="Change toxins")) %>% 
  mutate(`ICES_medium_location(s)` = recode(`ICES_medium_location(s)`,
                                            "1" ="1", "2"="2","3"="3","4"="4",
                                            "5"="5","6"="6","7"="7","Norvegian_coast" = "Norwegian coast",
                                            "Not_specified" = "Not specified", "Whole_North_Sea" = "Whole North Sea")) %>% 
  group_by( `Analyzed impact(s)`,`ICES_medium_location(s)`)%>%
  count()

dat_a_i1 <- dat_a_i %>% 
  group_by(`Analyzed impact(s)`) %>% mutate(sum_p = sum(n))

ggplot(dat_a_i1, mapping =aes(x = reorder(`Analyzed impact(s)`,-sum_p), y = n, fill = `ICES_medium_location(s)`))+
  geom_col()+
  scale_fill_brewer(palette = "Paired")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "", y = "Number of papers")+
  theme_test() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")+ 
  guides(fill=guide_legend(title="ICES medium locations"))

#3.nature of study population ----
dat_study_pop <- answers_meta_final%>%
  filter(year != 2021)%>%  
  select(nr, `Nature of the study population`, `ICES_medium_location(s)`)%>%
  separate_longer_delim(`Nature of the study population`, delim = "|||")%>%
  separate_longer_delim(`ICES_medium_location(s)`, delim = "|||") %>% 
  mutate(`Nature of the study population` = recode(`Nature of the study population`, 
                                                   "Benthos" ="Benthos","Chemical"="Chemical","Macropthytes"="Macropthytes","Phytoplankton"="Macropthytes",
                                                   "Physical"="Physical","Bacteria_other_microorganisms"="Bacteria other microorganisms", "Seabirds"="Seabirds","Fish"="Fish",
                                                   "Zooplankton"="Zooplankton","Marine_mammals"="Marine mammals","All"="All","Other"="Other",                       
                                                   "Not_applicable"="Not applicable","Cephalopods"="Cephalopods")) %>% 
  mutate(`ICES_medium_location(s)` = recode(`ICES_medium_location(s)`,
                                            "1" ="1", "2"="2","3"="3","4"="4",
                                            "5"="5","6"="6","7"="7","Norvegian_coast" = "Norwegian coast",
                                            "Not_specified" = "Not specified", "Whole_North_Sea" = "Whole North Sea")) %>% 
  group_by( `Nature of the study population`,`ICES_medium_location(s)`)%>%
  count()

dat_study_pop1 <- dat_study_pop %>% 
  group_by(`Nature of the study population`) %>% mutate(sum_p = sum(n)) 

ggplot(dat_study_pop1, mapping =aes(x = reorder(`Nature of the study population`,-sum_p), y = n, fill = `ICES_medium_location(s)`))+
  geom_col()+
  scale_fill_brewer(palette = "Paired")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x = "", y = "Number of papers")+
  theme_test() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")+ 
  guides(fill=guide_legend(title="ICES medium locations"))
