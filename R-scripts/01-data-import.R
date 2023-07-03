

### plotting growth curves from yeast data

library(tidyverse)
library(readxl)
library(cowplot)
theme_set(theme_cowplot())


plate <- read_excel("data-raw/June1623_30C.xlsx", range = "A40:CL137")

plate2 <- plate %>% 
  filter(`Time [s]` != "Temp. [°C]") %>% 
  gather(2:90, key = time, value = OD) %>% 
  rename(well = `Time [s]`) %>% 
  mutate(time = as.numeric(time))


### this is one way of assigning treatments to wells. Alternatively, you could list out all the wells in an csv or xls file with their treatments and then do a left join. This would be my preferred appraoach
plate3 <- plate2 %>% 
  mutate(treatment = case_when(str_detect(well, "A") ~ "wt",
                               str_detect(well, "B") ~ "ftz1",
                               str_detect(well, "C") ~ "ftz2",
                               str_detect(well, "D") ~ "ftz3",
                               str_detect(well, "E") ~ "casp1",
                               str_detect(well, "F") ~ "casp2",
                               str_detect(well, "G") ~ "casp3",
                               str_detect(well, "H1|H2|H3") ~ "blank",
                               TRUE ~ "water")) ### this is a shortcut to assign all the remaining wells to water

## plot them all together
plate3 %>% 
  ggplot(aes(x = time, y = OD, group = well, color = treatment)) + geom_line() +

## plot in facets
plate3 %>% 
  ggplot(aes(x = time, y = OD, group = well, color = treatment)) + geom_line() +
  facet_wrap( ~ treatment)



