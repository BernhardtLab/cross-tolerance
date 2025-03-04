#01 - nlgab - growthrates
# Ijeoma Nwafor 
# march 3

library(readxl)
library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(janitor)
library(lubridate)
library(ggplot2)
library(dplyr)
library(scales) # for better axis breaks
library(hms)

# BLOCK 1 - 42C -----
#b133 - ct137 - results - have not added

#import well key (w) and data (d)
w <- read_excel("data-raw/well-plate-layout.xlsx", sheet = "block1")

d <- read_excel("data-raw/nglab_blocks/Nick_Feb1_25_Nglab_Block1_42C.xlsx", range = "B32:CU129")

d1 <- d %>% 
  gather(key = well, value = od_600, 3:98)

d2 <- left_join(d1, w, by = "well")  %>%
  clean_names()  

df <- d2 %>% 
  mutate(time = hms::as_hms(time)) %>% 
  filter(time > as_hms("00:14:00")) 

df$time <- as_hms(df$time_numeric)  # Or use as.POSIXct depending on the format of your data

# red line is the average geom_smooth line with the loess method
ggplot(df, aes(x = time, y = od_600, color = strain)) + 
  geom_point()+
  geom_smooth(method = "loess", se = TRUE, color = "red") + 
  facet_wrap(~strain, scales = "free_y") +
  theme(legend.position = "none") +
  ggtitle("Feb1_25_Nglab_Block1_42C") +
  scale_x_time(
    breaks = as_hms(seq(min(df$time_numeric), max(df$time_numeric), by = 3600)),  #seconds
    labels = scales::time_format("%H:%M"))

# OKay awesome! we have some raw plotted od_600 values for block 1 - lets think of our next steps 
# 1. Calculate growth rates - pick out one population to work with right now  40_C7 or WT_YPD_1
#  use this package to get GR and carrying capacity https://tpetzoldt.github.io/growthrates/doc/Introduction.html
# 2. clean up cross tolerance folder  - done mar 3rd
# 3. Upload all data to folder + make note of alerts Nick made
# 4. loop through all blocks / temps

