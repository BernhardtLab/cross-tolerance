### Summarise and visualize growth rates across blocks and temperatures

# Author: Joey Bernhardt
# Description: Reads the combined growth rate data produced by
#              09-growth-rates-no-lag.R and generates summary statistics
#              and visualizations of growth rates by evolution history,
#              temperature, and block.
# Input: "data-processed/all-blocks-growth-no-lag.csv"
# Output: figures/growth-41-fluconazole.png
#         figures/growth-35-caspofungin.png
#         figures/all-growth-rates.png
#         figures/all-growth-rates-same-scale-grid.png
#         figures/all-growth-rates-same-scale-wrap.png
#         figures/all-growth-rates-wrap.png
#         figures/all-growth-rates-wrap-no.png
#         figures/growth-rates-temp.png
# Requires: 09-growth-rates-no-lag.R (produces input file)
# Written for R version 4.2.3
# Last updated: April 01 2026

# load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)
theme_set(theme_cowplot())
library(plotrix)


# read in data ------------------------------------------------------------

all_blocks_no_lag <- read_csv("data-processed/all-blocks-growth-no-lag.csv")
all_blocks_no_lag_prefactor <- read_csv("data-raw/all-blocks-growth-no-lag-prefactor.csv")
# 
# reference  <- arrange(all_blocks_no_lag_prefactor, block, test_temperature, strain)
# refactored <- arrange(all_blocks_no_lag, block, test_temperature, strain)
# 
# # Detailed comparison
# waldo::compare(reference, refactored) ### this is a sanity check for me to make sure that the outputs after refactoring the code are the same as in my kludgy version, and they are! woohoo.
# all.equal(reference, refactored)




all.equal(all_blocks_no_lag, all_blocks_no_lag_prefactor)

# fluconazole and caspofungin diagnostic plots ----------------------------

all_blocks_no_lag %>%
  filter(test_temperature == 41) %>%
  filter(evolution_history == "Fluconazole evolved") %>%
  ggplot(aes(x = strain, y = mu)) + geom_point() +
  ggtitle("Growth rates at 41C")
ggsave("figures/growth-41-fluconazole.png", width = 8, height = 6)

all_blocks_no_lag %>%
  filter(test_temperature == 35) %>%
  filter(evolution_history == "Caspofungin evolved") %>%
  ggplot(aes(x = strain, y = mu, color = factor(block))) + geom_point() +
  ggtitle("Growth rates at 35C")
ggsave("figures/growth-35-caspofungin.png", width = 8, height = 6)


# summarise by evolution history, temperature, and block ------------------

all_blocks_no_lag %>%
  ggplot(aes(x = evolution_history, y = mu, color = factor(block))) + geom_point()

all_sum <- all_blocks_no_lag %>%
  group_by(evolution_history, test_temperature, block) %>%
  summarise(mean_growth_rate = mean(mu),
            se_growth_rate = std.error(mu))

all_sum %>%
  ggplot(aes(x = evolution_history, y = mean_growth_rate, color = factor(block))) + geom_point(size = 4) +
  geom_errorbar(aes(x = evolution_history, ymin = mean_growth_rate - se_growth_rate, ymax = mean_growth_rate + se_growth_rate), width = 0.4) +
  facet_wrap(~ test_temperature, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave("figures/all-growth-rates.png", width = 14, height = 6)

all_sum %>%
  ggplot(aes(x = evolution_history, y = mean_growth_rate, color = evolution_history)) + geom_point() +
  geom_errorbar(aes(x = evolution_history, ymin = mean_growth_rate - se_growth_rate, ymax = mean_growth_rate + se_growth_rate), width = 0.1) +
  facet_grid(test_temperature ~ block) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave("figures/all-growth-rates-same-scale-grid.png", width = 14, height = 6)


# summarise across blocks -------------------------------------------------

all_sum2 <- all_sum %>%
  dplyr::select(-se_growth_rate) %>%
  ungroup() %>%
  mutate(test_temperature = as.factor(test_temperature)) %>%
  dplyr::select(-block)

all_sum3 <- all_sum2 %>%
  group_by(evolution_history, test_temperature) %>%
  summarise(mean_growth_rate2 = mean(mean_growth_rate),
            se_growth_rate = plotrix::std.error(mean_growth_rate))

all_sum3 %>%
  ggplot(aes(x = evolution_history, y = mean_growth_rate2, color = evolution_history)) + geom_point() +
  geom_errorbar(aes(x = evolution_history, ymin = mean_growth_rate2 - se_growth_rate, ymax = mean_growth_rate2 + se_growth_rate), width = 1) +
  facet_wrap(~ test_temperature, scales = "free") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave("figures/all-growth-rates-same-scale-wrap.png", width = 14, height = 6)

all_sum3 %>%
  ggplot(aes(x = evolution_history, y = mean_growth_rate2, color = evolution_history)) + geom_point() +
  geom_errorbar(aes(x = evolution_history, ymin = mean_growth_rate2 - se_growth_rate, ymax = mean_growth_rate2 + se_growth_rate), width = 1) +
  facet_wrap(~ test_temperature) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
ggsave("figures/all-growth-rates-wrap.png", width = 14, height = 6)

all_sum3b <- all_sum3 %>%
  mutate(test_temperature = as.numeric(as.character(test_temperature)))

all_sum3b %>%
  ggplot(aes(x = test_temperature, y = mean_growth_rate2, color = evolution_history)) + geom_point() +
  geom_errorbar(aes(x = test_temperature, ymin = mean_growth_rate2 - se_growth_rate, ymax = mean_growth_rate2 + se_growth_rate), width = 1)
ggsave("figures/all-growth-rates-wrap-no.png", width = 14, height = 6)


# growth rates vs temperature (individual + mean) -------------------------

ab2 <- all_blocks_no_lag %>%
  filter(!evolution_history %in% c("Caspofungin evolved", "Fluconazole evolved")) %>%
  mutate(test_temperature = as.numeric(as.character(test_temperature)))

ggplot() +
  geom_jitter(aes(x = test_temperature, y = mu, color = evolution_history), data = ab2, alpha = 0.6) +
  geom_point() +
  geom_pointrange(aes(x = test_temperature, y = mean_growth_rate2,
                      ymin = mean_growth_rate2 - se_growth_rate,
                      ymax = mean_growth_rate2 + se_growth_rate,
                      color = evolution_history),
                  position = position_dodge2(width = 2),
                  data = filter(all_sum3b, !evolution_history %in% c("Caspofungin evolved", "Fluconazole evolved")),
                  size = 1) +
  geom_pointrange(aes(x = test_temperature, y = mean_growth_rate2,
                      ymin = mean_growth_rate2 - se_growth_rate,
                      ymax = mean_growth_rate2 + se_growth_rate),
                  shape = 21, color = "black",
                  position = position_dodge2(width = 2),
                  data = filter(all_sum3b, !evolution_history %in% c("Caspofungin evolved", "Fluconazole evolved")),
                  size = 1) +
  ylab("Growth rate (per day)") +
  xlab("Temperature (C)")
ggsave("figures/growth-rates-temp.png", width = 8, height = 6)
