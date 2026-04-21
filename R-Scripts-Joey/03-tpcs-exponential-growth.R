

### fit TPCs to the exponential fit growth rates

# packages ----------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(rTPC)
library(nls.multstart)
library(broom)
library(plotrix)
library(conflicted)

theme_set(theme_cowplot())

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")




high_temp_growth <- read_csv("data-processed/growth-rates-sequential-logscale-41-42C.csv") |> 
  separate(rep.id, into = c("well", "block", "test_temperature", "evolved_temperature", "evolved well", "evolution_history"), remove = FALSE, sep = "_") |> 
  rename(r = r_final)
other_temps_growth <- read_csv("data-processed/growth-rates-exponential.csv") |> 
  separate(rep.id, into = c("well", "block", "test_temperature", "evolved_temperature", "evolved well", "evolution_history"), remove = FALSE, sep = "_") |> 
  filter(!test_temperature %in% c("41", "42"))


all_temps_growth <- bind_rows(other_temps_growth, high_temp_growth)

all_temps_growth |> ### ok something is looking weird here... the high temperature growth rates are really low.
  ggplot(aes(x = test_temperature, y = r)) + geom_point()



