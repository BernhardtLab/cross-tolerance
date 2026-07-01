

### compare AUC TPC traits and IC50s
library(tidyverse)
library(readxl)
library(cowplot)
library(conflicted)
# Prefer dplyr's select function
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
theme_set(theme_cowplot())

tpc_traits <- read_csv("data-processed/gcplyr/tpc-params-auc-gcplyr-17.csv")
ics <- read_csv("data-processed/all-ic50-estimates.csv")


all_traits <- left_join(ics, tpc_traits, by = c("population" = "strain", "evolution_history"))

write_csv(all_traits, "data-processed/all-traits.csv")

all_traits |> 
  # filter(drug == "amphotericin") |> 
  ggplot(aes(x = log(ic50), y = tmax, color = evolution_history.y)) + geom_point() +
  geom_smooth(method = "lm") +facet_wrap( ~ drug, scales = "free") +ylab("Tmax, degree C")
ggsave("figures/tmax-ic50-colours.png", width = 10, height = 4)


all_traits |> 
  # filter(drug == "amphotericin") |> 
  ggplot(aes(x = tmax, y = log(ic50))) + geom_point() +
  geom_smooth(method = "lm") +facet_wrap( ~ drug, scales = "free") +ylab("IC50")
ggsave("figures/ic5-tmax.png", width = 10, height = 4)


all_traits |> 
  # filter(drug == "amphotericin") |> 
  ggplot(aes(x = th, y = log(ic50), color = evolution_history)) + geom_point() +
  geom_smooth(method = "lm", color = "black") +facet_wrap( ~ drug, scales = "free") +ylab("IC50") +
  scale_color_manual(values = c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000"))
ggsave("figures/ic5-th.png", width = 10, height = 4)




amph_data <- all_traits |> 
  filter(drug == "amphotericin") 
moda <- lm(log(ic50) ~ th, data = amph_data)
summary(moda)

amph_data |> 
  ggplot(aes(x = th, y = log(ic50))) + geom_point() +
  geom_smooth(method = "lm")

amph_data |> 
  ggplot(aes(x = eh, y = log(ic50))) + geom_point() +
  geom_smooth(method = "lm")


amph_data <- all_traits |> 
  filter(drug == "amphotericin")
  # # filter(evolution_history == "40 evolved") |> 
  # filter(ic50 > 2) ### looks like there is a big outlier here -- not sure what's going on here

modf <- lm(log(ic50) ~ th + evolution_history, data = amph_data)
summary(modf)


library(smatr)

fit <- sma(log(ic50) ~ th, data = amph_data)
summary(fit)


# does the slope itself differ between temperature regimes?
fit_grp <- sma(log(ic50) ~ th*evolution_history, data = amph_data)
summary(fit_grp)

# if slopes don't differ, fit one common slope and test elevation shift
fit_common <- sma(log(ic50) ~ tmax, data = amph_data)
summary(fit_common)


# fluconazole -------------------------------------------------------------

fluc_data <- all_traits |> 
  filter(drug == "fluconazole")
# # filter(evolution_history == "40 evolved") |> 
# filter(ic50 > 2) ### looks like there is a big outlier here -- not sure what's going on here

modf <- lm(log(ic50) ~ tmax, data = fluc_data)
summary(modf)


library(smatr)

fit <- sma(log(ic50) ~ tmax, data = fluc_data)
summary(fit)


# does the slope itself differ between temperature regimes?
fit_grp <- sma(log(ic50) ~ tmax*evolution_history, data = fluc_data)
summary(fit_grp)

# if slopes don't differ, fit one common slope and test elevation shift
fit_common <- sma(log(ic50) ~ tmax, data = fluc_data)
summary(fit_common)