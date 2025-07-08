

### plotting the spot plates


library(tidyverse)
library(readxl)
library(janitor)
library(cowplot)
theme_set(theme_cowplot())

plate41C_Oct <- read_excel("data-raw/Spot plates quanitification.xlsx", sheet = "Oct29-31.24.AllG10_41C") %>% 
  clean_names() %>% 
  filter(!grepl("d40", population)) %>% 
  filter(!grepl("df", population)) %>% 
  mutate(evolution_history = case_when(grepl("35", population) ~ "35 evolved",
                                       grepl("40", population) ~ "40 evolved",
                                       grepl("FLZ", population) ~ "Fluconazole evolved",
                                       grepl("CASP", population) ~ "Caspofungin evolved",
                                       TRUE ~ population)) %>% 
  mutate(test_temperature = 41) %>% 
  mutate(block = 0)

str(plate41C_Oct)


plate41C_Oct %>% 
  ggplot(aes(x = position, y = area_pixels, color = evolution_history)) + geom_point()

plate41C_Oct %>% 
  group_by(evolution_history, position, dilution) %>% 
  summarise(mean_area = mean(area_pixels),
            se_area = std.error(area_pixels)) %>% 
  ggplot(aes(x = (dilution), y = mean_area, color = evolution_history)) + geom_point() +
  geom_errorbar(aes(x = (dilution), ymin = mean_area - se_area, ymax = mean_area + se_area)) +
  scale_x_log10()
ggsave("figures/spot-plates-oct.png", width = 8, height = 6)


plate41C_Jan5 <- read_excel("data-raw/Spot plates quanitification.xlsx", sheet = "REP 1 Jan5-7.25.Nglab.G10.All_G") %>% 
  clean_names() %>% 
  filter(!grepl("d40", population)) %>% 
  filter(!grepl("df", population)) %>% 
  filter(!grepl("LIG4", population))  %>% 
  filter(!grepl("YPD", population))  %>% 
  mutate(evolution_history = case_when(grepl("35", population) ~ "35 evolved",
                                       grepl("40", population) ~ "40 evolved",
                                       grepl("FLZ", population) ~ "Fluconazole evolved",
                                       grepl("CASP", population) ~ "Caspofungin evolved",
                                       TRUE ~ population)) %>% 
  mutate(test_temperature = 41) %>% 
  mutate(block = 1)


plate41C_Jan5 %>% 
  filter(evolution_history %in% c("35 evolved", "40 evolved")) %>% 
  group_by(evolution_history, position, dilution) %>% 
  summarise(mean_area = mean(area_pixels),
            se_area = std.error(area_pixels)) %>% 
  ggplot(aes(x = (dilution), y = mean_area, color = evolution_history)) +
  # geom_point() +
  # geom_errorbar(aes(x = (dilution), ymin = mean_area - se_area, ymax = mean_area + se_area)) +
  scale_x_log10() +
  geom_pointrange(aes(x = dilution, y = mean_area, ymin = mean_area - se_area, ymax = mean_area + se_area), position = position_dodge(width = .5))
ggsave("figures/spot-plates-jan5.png", width = 8, height = 6)



plate41C_Jan5_r2 <- read_excel("data-raw/Spot plates quanitification.xlsx", sheet = "REP 2 Jan5-7.25.Nglab.G10.All_G") %>% 
  clean_names() %>% 
  filter(!grepl("d40", population)) %>% 
  filter(!grepl("df", population)) %>% 
  filter(!grepl("LIG4", population)) %>% 
  filter(!grepl("YPD", population))  %>% 
  mutate(evolution_history = case_when(grepl("35", population) ~ "35 evolved",
                                       grepl("40", population) ~ "40 evolved",
                                       grepl("FLZ", population) ~ "Fluconazole evolved",
                                       grepl("CASP", population) ~ "Caspofungin evolved",
                                       TRUE ~ population)) %>% 
  mutate(test_temperature = 41) %>% 
  mutate(block = 2)


plate41C_Jan5_r2 %>% 
  filter(evolution_history %in% c("35 evolved", "40 evolved")) %>% 
  group_by(evolution_history, position, dilution) %>% 
  summarise(mean_area = mean(area_pixels),
            se_area = std.error(area_pixels)) %>% 
  ggplot(aes(x = (dilution), y = mean_area, color = evolution_history)) +
  # geom_point() +
  # geom_errorbar(aes(x = (dilution), ymin = mean_area - se_area, ymax = mean_area + se_area)) +
  scale_x_log10() +
  geom_pointrange(aes(x = dilution, y = mean_area, ymin = mean_area - se_area, ymax = mean_area + se_area), position = position_dodge(width = .5))
ggsave("figures/spot-plates-jan5r2.png", width = 8, height = 6)

plate41C_Jan5_r3 <- read_excel("data-raw/Spot plates quanitification.xlsx", sheet = "REP 3 Jan5-7.25.Nglab.G10.All_G") %>% 
  clean_names() %>% 
  filter(!grepl("d40", population)) %>% 
  filter(!grepl("df", population)) %>% 
  filter(!grepl("LIG4", population)) %>% 
  filter(!grepl("YPD", population))  %>% 
  mutate(evolution_history = case_when(grepl("35", population) ~ "35 evolved",
                                       grepl("40", population) ~ "40 evolved",
                                       grepl("FLZ", population) ~ "Fluconazole evolved",
                                       grepl("CASP", population) ~ "Caspofungin evolved",
                                       TRUE ~ population)) %>% 
  filter(!is.na(area_pixels)) %>% 
  mutate(test_temperature = 41)%>% 
  mutate(block = 3)

View(plate41C_Jan5_r3)

plate41C_Jan5_r3 %>% 
  filter(evolution_history %in% c("35 evolved", "40 evolved")) %>% 
  group_by(evolution_history, position, dilution) %>% 
  summarise(mean_area = mean(area_pixels),
            se_area = std.error(area_pixels)) %>% 
  ggplot(aes(x = (dilution), y = mean_area, color = evolution_history)) +
  # geom_point() +
  # geom_errorbar(aes(x = (dilution), ymin = mean_area - se_area, ymax = mean_area + se_area)) +
  scale_x_log10() +
  geom_pointrange(aes(x = dilution, y = mean_area, ymin = mean_area - se_area, ymax = mean_area + se_area), position = position_dodge(width = .5))
ggsave("figures/spot-plates-jan5r3.png", width = 8, height = 6)

plate35C_oct <- read_excel("data-raw/Spot plates quanitification.xlsx", sheet = "Oct17-19.Nglab.G10.35C") %>% 
  clean_names() %>% 
  filter(!grepl("d40", population)) %>% 
  filter(!grepl("df", population)) %>% 
  filter(!grepl("LIG4", population)) %>% 
  filter(!grepl("YPD", population))  %>% 
  mutate(evolution_history = case_when(grepl("35", population) ~ "35 evolved",
                                       grepl("40", population) ~ "40 evolved",
                                       grepl("FLZ", population) ~ "Fluconazole evolved",
                                       grepl("CASP", population) ~ "Caspofungin evolved",
                                       TRUE ~ population)) %>% 
  filter(!is.na(area_pixels)) %>% 
  mutate(test_temperature = 35) %>% 
  mutate(block = 1)

View(plate35C_oct)

plate35C_oct %>% 
  filter(evolution_history %in% c("35 evolved", "40 evolved")) %>% 
  group_by(evolution_history, position, dilution) %>% 
  summarise(mean_area = mean(area_pixels),
            se_area = std.error(area_pixels)) %>% 
  ggplot(aes(x = (dilution), y = mean_area, color = evolution_history)) +
  # geom_point() +
  # geom_errorbar(aes(x = (dilution), ymin = mean_area - se_area, ymax = mean_area + se_area)) +
  scale_x_log10() +
  geom_pointrange(aes(x = dilution, y = mean_area, ymin = mean_area - se_area, ymax = mean_area + se_area), position = position_dodge(width = .5))
ggsave("figures/spot-plates-35C-oct.png", width = 8, height = 6)


all_plates <- bind_rows(plate35C_oct, plate41C_Jan5, plate41C_Jan5_r2, plate41C_Jan5_r3)
write_csv(all_plates, "data-processed/all-spot-plates.csv")


all_plates <- read_csv("data-processed/all-spot-plates.csv")

all_plates %>% 
  filter(evolution_history %in% c("35 evolved", "40 evolved")) %>% 
  group_by(evolution_history, position, dilution, test_temperature) %>% 
  summarise(mean_area = mean(area_pixels),
            se_area = std.error(area_pixels)) %>% 
  ggplot(aes(x = (dilution), y = mean_area, color = evolution_history)) +
  facet_wrap( ~ test_temperature) +
  scale_x_log10() +
  geom_pointrange(aes(x = dilution, y = mean_area, ymin = mean_area - se_area, ymax = mean_area + se_area), position = position_dodge(width = .5))
ggsave("figures/spot-plates-all.png", width = 12, height = 6)


ap2 <- all_plates %>% 
  filter(dilution != 1) %>%
  filter(evolution_history != "(d)fRS585") %>%
  group_by(population, set, row, test_temperature, evolution_history, block) %>% 
  summarise(total_area = sum(area_pixels))

ap2 %>% 
  filter(!evolution_history %in% c("Caspofungin evolved", "Fluconazole evolved")) %>% 
  ggplot(aes(x = test_temperature, y = total_area, color = evolution_history)) + geom_jitter(width = .5)

library(plotrix)
ap3 <- ap2 %>% 
  filter(!evolution_history %in% c("Caspofungin evolved", "Fluconazole evolved")) %>% 
  group_by(evolution_history, test_temperature) %>% 
  summarise(mean_total_area = mean(total_area),
            se_total_area = std.error(total_area)) 

ap2 %>% 
  filter(!evolution_history %in% c("Caspofungin evolved", "Fluconazole evolved")) %>% 
  ggplot(aes(x = test_temperature, y = total_area, color = evolution_history)) + geom_jitter(width = .5, alpha = 0.5) + 
  
  geom_pointrange(aes(x = test_temperature, y = mean_total_area, ymin = mean_total_area - se_total_area, ymax = mean_total_area + se_total_area), data = ap3, position = position_dodge2(width = 0.5)) +
  geom_pointrange(aes(x = test_temperature, y = mean_total_area, ymin = mean_total_area - se_total_area, ymax = mean_total_area + se_total_area), shape = 21, color = "black", data = ap3, position = position_dodge2(width = 0.5)) + scale_color_brewer(palette = "Set2") +ylab("Total area") +
  xlab("Test temperature") +
  labs(color = "Evolution history")
ggsave("figures/spot-plates-temperature.png", width = 7, height = 5)


all_plates_sub <- all_plates %>% 
  filter(evolution_history %in% c("35 evolved", "40 evolved")) %>% 
  mutate(log_dilution = log10(dilution))

all_plates %>% 
  filter(evolution_history %in% c("35 evolved", "40 evolved")) %>%
  ggplot(aes(x = dilution, y = area_pixels, color = evolution_history)) +
  geom_point() +
  facet_wrap( ~ test_temperature) +
  scale_x_log10()

mod1 <- lm(area_pixels ~ evolution_history + test_temperature + dilution + evolution_history*test_temperature*dilution, data = all_plates_sub)
summary(mod1)  

all_plates_41 <- all_plates_sub %>% 
  filter(test_temperature == 41)

mod2 <- lm(mean_area ~ evolution_history, data = all_plates_40)
summary(mod2)  



# try mixed effects model -------------------------------------------------


library(nlme)

lmm_nlme <- lme(
  fixed = area_pixels ~ evolution_history * test_temperature * log_dilution,
  random = ~1 | population,
  data = all_plates_sub
)

summary(lmm_nlme)


library(emmeans)
library(ggplot2)

# Create grid of predicted values
emm <- emmeans(lmm_nlme, 
               ~ evolution_history | test_temperature * log_dilution,
               at = list(log_dilution = seq(log(1), log(1e-4), length.out = 10)),
               type = "response")

# Convert to data frame for plotting
emm_df <- as.data.frame(emm)

# Plot
ggplot(emm_df, aes(x = exp(log_dilution), y = emmean, 
                   color = evolution_history, 
                   group = evolution_history)) +
  geom_line(size = 1.2) +
  facet_wrap(~test_temperature) +
  scale_x_log10() +
  labs(
    x = "Dilution (log scale)",
    y = "Predicted Area (pixels)",
    color = "Evolution history",
    title = "Predicted area by dilution, test temperature, and evolution history"
  ) +
  theme_minimal()





all_plates_sub %>% 
  filter(log_dilution == -3) %>% 
  group_by(evolution_history) %>% 
  summarise(mean_area = mean(area_pixels),
            se_area = std.error(area_pixels)) %>% 
  ggplot(aes(x = evolution_history, y = mean_area)) + geom_point()

all_plates_sub$log_dilution_centered <- all_plates_sub$log_dilution + 4

lmm_nlme_centered <- lme(
  fixed = area_pixels ~ evolution_history * test_temperature * log_dilution_centered,
  random = ~1 | population,
  data = all_plates_sub
)

summary(lmm_nlme_centered)
emm <- emmeans(lmm_nlme_centered, ~ evolution_history | test_temperature * log_dilution_centered,
               at = list(log_dilution_centered = 0))

pairs(emm)

emmip(lmm_nlme_centered, evolution_history ~ log_dilution_centered | test_temperature)
               
library(emmeans)
dil_range <- seq(-2, 2, length.out = 100)

# Generate emmeans grid
emm <- emmeans(lmm_nlme_centered, 
               specs = ~ evolution_history * test_temperature * log_dilution_centered, 
               at = list(log_dilution_centered = dil_range), 
               type = "response")

emm_df <- as.data.frame(emm)

ggplot(emm_df, aes(x = log_dilution_centered, 
                   y = emmean, 
                   color = evolution_history)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = evolution_history), 
              alpha = 0.2, color = NA) +
  facet_wrap(~ test_temperature) +
  labs(
    title = "Predicted Area (pixels) across Dilution Gradient",
    x = "Centered log(Dilution)", 
    y = "Predicted Area (pixels)",
    linetype = "Test Temperature"
  ) +
  theme_minimal(base_size = 14)




