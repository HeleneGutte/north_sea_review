# Analysis for North Sea review ----

# 1. packages and data ----
library(mapplots)
library(tidyverse)
library(ggplot2)
answers_meta_final <- read_csv("Data/answers_meta_final.csv")

# 2. plotting settings ----
my_colours <- c("Climate change" = "orange2", "Direct exploitation" = "blue", 
                "Biological invasion" = "turquoise", "Pollution" = "purple", "Sea use change" = "gold", 
                "Global change" = "darkgreen"
)

# 3. time series plots ----
dat <- answers_meta_final%>%
  filter(year != 2021)%>%  
  select(year,`Anthropogenic driver(s)`, `Organizational level`)%>%
  separate_longer_delim(`Anthropogenic driver(s)`, delim = "|||")%>%
  separate_longer_delim(`Organizational level`, delim = "|||")%>%

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

ggplot (dat) +
  geom_line(aes(year, n))+
  facet_wrap(~`Anthropogenic driver(s)`+ `Organizational level`)+
  theme_test()



#order:
# 1. Pollution
# 2. Direct exploitation
# 3. Climate change
# 4. Sea use chage
# 5. Biological invasion
# 6. Global change

my_colours <- c("Climate change" = "orange2", "Direct exploitation" = "blue", 
                "Biological invasion" = "turquoise", "Pollution" = "purple", "Sea use change" = "gold", 
                "Global change" = "darkgreen"
)



#add colors----
#1. Pollution
dat_1 <- dat[dat$`Anthropogenic driver(s)` == "Pollution",]

colours <- NULL
colours[dat_1$`Organizational level` == "Ecosystem"]      <-"purple4"
colours[dat_1$`Organizational level` == "Community"]      <-"purple3"
colours[dat_1$`Organizational level` == "Population"]     <-"purple2"
colours[dat_1$`Organizational level` == "Individual"]     <-"purple1"
colours[dat_1$`Organizational level` == "Not_applicable"] <-"purple"
dat_1$colours <- colours

#2. Direct exploitation
dat_2 <- dat[dat$`Anthropogenic driver(s)` == "Direct_exploitation",]

colours <- NULL
colours[dat_2$`Organizational level` == "Ecosystem"]      <-"blue4"
colours[dat_2$`Organizational level` == "Community"]      <-"blue3"
colours[dat_2$`Organizational level` == "Population"]     <-"blue2"
colours[dat_2$`Organizational level` == "Individual"]     <-"blue1"
colours[dat_2$`Organizational level` == "Not_applicable"] <-"blue"
dat_2$colours <- colours

#3. Climate change
dat_3 <- dat[dat$`Anthropogenic driver(s)` == "Climate_change",]

colours <- NULL
colours[dat_3$`Organizational level` == "Ecosystem"]      <-"orange4"
colours[dat_3$`Organizational level` == "Community"]      <-"orange3"
colours[dat_3$`Organizational level` == "Population"]     <-"orange2"
colours[dat_3$`Organizational level` == "Individual"]     <-"orange1"
colours[dat_3$`Organizational level` == "Not_applicable"] <-"orange"
dat_3$colours <- colours

#4. Sea use change
dat_4 <- dat[dat$`Anthropogenic driver(s)` == "Sea_use_change",]

colours <- NULL
colours[dat_4$`Organizational level` == "Ecosystem"]      <-"gold4"
colours[dat_4$`Organizational level` == "Community"]      <-"gold3"
colours[dat_4$`Organizational level` == "Population"]     <-"gold2"
colours[dat_4$`Organizational level` == "Individual"]     <-"gold1"
colours[dat_4$`Organizational level` == "Not_applicable"] <-"gold"
dat_4$colours <- colours

#5. Biological invasion
dat_5 <- dat[dat$`Anthropogenic driver(s)` == "Biological_invasion",]

colours <- NULL
colours[dat_5$`Organizational level` == "Ecosystem"]      <-"turquoise4"
colours[dat_5$`Organizational level` == "Community"]      <-"turquoise3"
colours[dat_5$`Organizational level` == "Population"]     <-"turquoise2"
colours[dat_5$`Organizational level` == "Individual"]     <-"turquoise1"
colours[dat_5$`Organizational level` == "Not_applicable"] <-"turquoise"
dat_5$colours <- colours

#6. Biological invasion
dat_6 <- dat[dat$`Anthropogenic driver(s)` == "Global_change",]

colours <- NULL
colours[dat_6$`Organizational level` == "Ecosystem"]      <-"darkolivegreen"
colours[dat_6$`Organizational level` == "Community"]      <-"darkolivegreen4"
colours[dat_6$`Organizational level` == "Population"]     <-"darkolivegreen3"
colours[dat_6$`Organizational level` == "Individual"]     <-"darkolivegreen2"
colours[dat_6$`Organizational level` == "Not_applicable"] <-"darkolivegreen1"
dat_6$colours <- colours

#combine datasets again
dat_all <- rbind(dat_1,dat_2,dat_3,dat_4,dat_5,dat_6)
col <- dat_all$colours

#add levels
dat_all$`Organizational level` <- factor(dat_all$`Organizational level`, levels = c("Ecosystem","Community","Population","Individual","Not_applicable"))


#line_plots----
ggplot (dat_all) +
  geom_line(aes(year, n, col =col))+
  facet_wrap(~`Anthropogenic driver(s)`+ `Organizational level`)+
  labs(x="Year", y="nr of papers")+
  theme_test()

#barplot----

ggplot(dat_all)+
  geom_bar(aes(x=factor(`Anthropogenic driver(s)`,levels = c("Pollution","Direct_exploitation","Climate_change",
                                                             "Sea_use_change","Biological_invasion","Global_change")),
               y = n, group=`Organizational level`), 
               stat="identity", fill = col)+
  coord_flip()+
  labs(x="Anthropogenic driver", y = "Number paper")+
  theme_test()


#pie chart ----
dat <- answers_meta_final%>%
  filter(year != 2021)%>%  
  select(year,`Anthropogenic driver(s)`, `Organizational level`)%>%
  separate_longer_delim(`Anthropogenic driver(s)`, delim = "|||")%>%
  separate_longer_delim(`Organizational level`, delim = "|||")%>%
  
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

ggplot(dat)+
  geom_bar(aes(`Anthropogenic driver(s)`, fill= `Organizational level` ), position = "fill") +
  coord_polar()

           