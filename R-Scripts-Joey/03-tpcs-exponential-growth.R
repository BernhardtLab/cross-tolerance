

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




high_temp_growth <- read_csv("data-processed/growth-rates-sequential-logscale-41-42C.csv")