#SVETA

library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(cowplot)

#GRAPHING OD READS FROM HYS_1 DAY 1
hys1_37 <- read_excel ("data-raw/HYS_1/HYS_1.xlsx", sheet = "37") %>% 
  mutate(temperature = 37)
view(hys1_37)
#on its own
hys1_37 %>% 
 ggplot(aes(x = well, y = day1)) + geom_point()

hys1_42  <- read_excel ("data-raw/HYS_1/HYS_1.xlsx", sheet = "42") %>% 
  mutate(temperature = 42)
view(hys1_42)
#on its own
hys1_42 %>% 
  ggplot(aes(x = well, y = day1)) + geom_point()

#together
hys1_both <- bind_rows(hys1_42, hys1_37) %>% 
  mutate(unique_well = paste(well,temperature, sep = "_")) %>% view()

hys1_both %>% 
  ggplot (aes (x = well, y = day1, colour = factor(temperature), group = unique_well)) + 
  geom_point() +
  xlab('well') +
  ylab('OD600')
#blank = well h12
#blank values close together --> overlap --> looks as if only one dot, but with zoom in it's two
#blank will not be h12 from now on (prob should change h12 --> blank in future?)


#GRAPHING OD READS FROM HYS_2 DAY 1
hys2_37 <- read_excel ("data-raw/HYS2/HYS2.xlsx", sheet = "37") %>% 
  mutate(temperature = 37)
View(hys2_37)

hys2_37 %>% 
  ggplot(aes(x = well, y = day1, colour = factor(treatment))) + 
  geom_point() +
  ggtitle('37C')
  
hys2_42 <- read_excel ("data-raw/HYS2/HYS2.xlsx", sheet = "42") %>% 
  mutate(temperature = 42)
View(hys2_42)

hys2_42 %>% 
  ggplot(aes(x = well, y = day1, colour = factor(treatment))) + 
  geom_point() +
  ggtitle('42C')

#both together
hys2_both <- bind_rows(hys2_42, hys2_37) %>% 
  mutate(unique_well = paste(well,temperature, sep = "_"))
View(hys2_both)

hys2_both %>% 
  ggplot (aes (x = well, y = day1, colour = factor(temperature), group = unique_well)) + 
  geom_point() +
  ggtitle('both')
#didn't colour code blanks vs culture, but evident that lowest od simlar in both temps is blanks

#GRAPHING OD READS FROM HYS_2 DAY 2
#graphing 42 - looking at difference between day 1 and day 2
hys2_42alt <- read_excel ("data-raw/HYS2/HYS2.xlsx", sheet = "42alt") %>% 
  mutate(temperature = 42) %>% 
  mutate(unique_well = paste(well,day, sep = "_"))
View(hys2_42alt)

hys2_42alt %>% 
  ggplot(aes( x = well, y = OD, colour = factor(day), group = unique_well))+
  geom_point() +
  ggtitle('42 days 1 and 2') #separates by day

#grpahing 37
hys2_37alt <- read_excel ("data-raw/HYS2/HYS2.xlsx", sheet = "37alt") %>% 
  mutate(temperature = 37) %>% 
  mutate(unique_well = paste(well,day, sep = "_"))
View(hys2_37alt)

hys2_37alt %>% 
  ggplot(aes( x = well, y = OD, colour = factor(day), group = unique_well))+
  geom_point() +
  ggtitle('37 days 1 and 2') #separates by day

#separating by treatment
hys2_42alt <- read_excel ("data-raw/HYS2/HYS2.xlsx", sheet = "42alt") %>% 
  mutate(temperature = 42) %>% 
  mutate(unique_well = paste(well,day,treatment, sep = "_"))
View(hys2_42alt)

hys2_42alt %>% 
  ggplot(aes( x = well, y = OD, colour = factor(day), group = unique_well))+
  geom_point(aes(shape = treatment), size = 4) +
  theme_minimal() +
  ggtitle('42 days 1 and 2') #separates by day and treatment

hys2_37alt <- read_excel ("data-raw/HYS2/HYS2.xlsx", sheet = "37alt") %>% 
  mutate(temperature = 37) %>% 
  mutate(unique_well = paste(well,day,treatment, sep = "_"))
View(hys2_37alt)

hys2_37alt %>% 
  ggplot(aes( x = well, y = OD, colour = factor(day), group = unique_well))+
  geom_point(aes(shape = treatment), size = 4) +
  theme_minimal() +
  ggtitle('37 days 1 and 2') #separates by day and treatment

#both days
hys2_bothalt <- bind_rows(hys2_42alt, hys2_37alt) 
View(hys2_bothalt)

#hys2_bothalt %>% 
#  ggplot(aes(x = well, y = OD, colour = factor(day), group = unique_well)) +
#  geom_point (fill = "black",
#              size = 4,
#              shape = 21)

hys2_bothalt %>% 
  ggplot(aes(x = well, y = OD, colour = day, group = unique_well)) +
  scale_shape_manual(values = c(21, 23)) +
  geom_point (aes(shape = treatment, fill = temperature), size = 4, ) +
  theme_minimal() +
  ggtitle("HYS2 days 1 and 2, 37 and 42 deg") #separates by day, treatment, temp

#joey said to fix the temp scale in legend, do factor(temperature)
hys2_bothalt %>% 
  ggplot(aes(x = well, y = OD, colour = day, group = unique_well)) +
  scale_shape_manual(values = c(21, 23)) +
  geom_point (aes(shape = treatment, fill = factor(temperature)), size = 4, ) +
  theme_minimal() +
  ggtitle("HYS2 days 1 and 2, 37 and 42 deg") #but how change colour for temp??

install.packages("wesanderson")
library(wesanderson)
install.packages("hrbrthemes")
library(hrbrthemes) # for plot themes
#library(gapminder) # for data
#library(ggbump) # for the bump plot

#?wes_palette
#?`wesanderson-package`
#?wes_palette

hys2_bothalt %>% 
  ggplot(aes(x = well, y = OD, colour = day, group = unique_well)) +
  scale_shape_manual(values = c(21, 23)) +
  geom_point (aes(shape = treatment, size = 4, fill = factor(temperature),  stroke = 2))+
  scale_fill_manual(values=wes_palette(n=2, name="GrandBudapest2")) +
  scale_colour_manual(values=wes_palette(n=2, name="Cavalcanti1")) +
  theme_minimal() +
  ggtitle("HYS2 days 1 and 2, 37 and 42 deg")
#the legend for factor(temperature) doesn't match what's on the graph tho...

#HYS2 DAY3 GRAPHS
hys2_42alt <- read_excel ("data-raw/HYS2/HYS2.xlsx", sheet = "42alt") %>% 
  mutate(temperature = 42) %>% 
  mutate(unique_well = paste(well,day,treatment, sep = "_"))
View(hys2_42alt)

hys2_42alt %>% 
  ggplot(aes( x = well, y = OD, colour = factor(day), group = unique_well))+
  geom_point(aes(shape = treatment), size = 4) +
  theme_minimal() +
  ggtitle('42 days 1, 2, 3')

hys2_37alt <- read_excel ("data-raw/HYS2/HYS2.xlsx", sheet = "37alt") %>% 
  mutate(temperature = 37) %>% 
  mutate(unique_well = paste(well,day,treatment, sep = "_"))
View(hys2_37alt)

hys2_37alt %>% 
  ggplot(aes( x = well, y = OD, colour = factor(day), group = unique_well))+
  geom_point(aes(shape = treatment), size = 4) +
  theme_minimal() +
  ggtitle('37 days 1, 2, 3')

#day on x axis and facet wrap by well
hys2_42alt %>% 
  ggplot(aes( x = day, y = OD, colour = treatment, group = unique_well))+
  geom_point(aes(shape = treatment), size = 2) +
  theme_minimal() +
  facet_wrap(~well, scales = "free_y") +
  ggtitle('42 days 1, 2, 3')

hys2_37alt %>% 
  ggplot(aes( x = day, y = OD, colour = treatment, group = unique_well))+
  geom_point(aes(shape = treatment), size = 2) +
  theme_minimal() +
  facet_wrap("well") +
  geom_line(group_by = "well") +
  ggtitle('37 days 1, 2, 3')


##Ijeoma Nwaofr
# September 1st, 2023


##HYS 2 Day 4
#37C
sept1_37 <- read_excel ("data-raw/HYS2/HYS2.xlsx", sheet = "37alt") %>% 
  mutate(temperature = 37) %>% 
  mutate(unique_well = paste(well,day,treatment, sep = "_"))

sept1_37 %>% 
  ggplot(aes( x = well, y = OD, colour = factor(day), group = unique_well))+
  geom_point(aes(shape = treatment), size = 4) +
  theme_minimal() +
  ggtitle('37 days 1-4')

sept1_37 %>% 
  ggplot(aes( x = day, y = OD, colour = treatment, group = unique_well))+
  geom_point(aes(shape = treatment), size = 2) +
  theme_minimal() +
  facet_wrap(~well, scales = "free_y") +
  geom_line(aes(x = day, y = OD, group = well)) +
  ggtitle('37 days 1-4')

#42C
sept1_42 <- read_excel ("data-raw/HYS2/HYS2.xlsx", sheet = "42alt") %>% 
  mutate(temperature = 42) %>% 
  mutate(unique_well = paste(well,day,treatment, sep = "_"))

sept1_42 %>% 
  ggplot(aes( x = well, y = OD, colour = factor(day), group = unique_well))+
  geom_point(aes(shape = treatment), size = 4) +
  theme_minimal() +
  ggtitle('42 Days 1 - 4')

sept1_42 %>% 
  ggplot(aes( x = day, y = OD, colour = treatment, group = unique_well))+
  geom_point(aes(shape = treatment), size = 2) +
  theme_minimal() +
  facet_wrap(~well, scales = "free_y") +
  geom_line(aes(x = day, y = OD, group = well)) +
  ggtitle('42 days 1-4')

##Sept 2nd - HYS 2 Day 5
#37C
sept2_37 <- read_excel ("data-raw/HYS2/HYS2.xlsx", sheet = "37alt") %>% 
  mutate(temperature = 37) %>% 
  mutate(unique_well = paste(well,day,treatment, sep = "_"))

sept2_37 %>% 
  ggplot(aes( x = well, y = OD, colour = factor(day), group = unique_well))+
  geom_point(aes(shape = treatment), size = 4) +
  theme_minimal() +
  ggtitle('37 days 1-5')

sept2_37 %>% 
  ggplot(aes( x = day, y = OD, colour = treatment, group = unique_well))+
  geom_point(aes(shape = treatment), size = 2) +
  theme_minimal() +
  facet_wrap(~well, scales = "free_y") +
  geom_line(aes(x = day, y = OD, group = well)) +
  ggtitle('37 days 1-5')

#42C
sept2_42 <- read_excel ("data-raw/HYS2/HYS2.xlsx", sheet = "42alt") %>% 
  mutate(temperature = 42) %>% 
  mutate(unique_well = paste(well,day,treatment, sep = "_"))

sept2_42 %>% 
  ggplot(aes( x = well, y = OD, colour = factor(day), group = unique_well))+
  geom_point(aes(shape = treatment), size = 4) +
  theme_minimal() +
  ggtitle('42 Days 1 - 5')

sept2_42 %>% 
  ggplot(aes( x = day, y = OD, colour = treatment, group = unique_well))+
  geom_point(aes(shape = treatment), size = 2) +
  theme_minimal() +
  facet_wrap(~well, scales = "free_y") +
  geom_line(aes(x = day, y = OD, group = well)) +
  ggtitle('42 days 1-5')
