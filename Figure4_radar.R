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
  facet_wrap(~`Anthropogenic driver(s)`+ `Organizational level`)

#4. pie chart ----
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

           