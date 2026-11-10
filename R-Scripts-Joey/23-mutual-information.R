library(tidyverse)

EVO_COLORS <- c("40 evolved" = "#FA3208", "35 evolved" = "#0E63FF", "fRS585" = "#000000")

OUT_TPC <- "data-processed/gcplyr"
OUT     <- "data-processed"

### ic50 values
ic50_strains <- read_csv(file.path(OUT, "normalised-ic50-per-strain.csv"))

                         
tpc_params  <- read_csv(file.path(OUT_TPC, "tpc-params-auc-gcplyr-17.csv"))

all_traits <- left_join(ic50_strains, tpc_params, by = c("population" = "strain"))

fluc <- all_traits |> 
  filter(drug == "fluconazole")

### mutual information: log_ratio vs th_c
library(FNN)

# KSG (k-nearest-neighbour) estimator avoids arbitrary binning with small n;
# variables are standardised because KSG uses the max-norm across dimensions
mi_dat <- fluc |> 
  select(log_ratio, th_c) |> 
  drop_na() |> 
  mutate(across(everything(), \(v) as.numeric(scale(v))))

k_nn <- 3
mi_obs <- FNN::mutinfo(mi_dat$log_ratio, mi_dat$th_c, k = k_nn)  # nats

# Permutation null: shuffling breaks any dependence between the two traits
set.seed(7316)
mi_null <- replicate(
  5000,
  FNN::mutinfo(mi_dat$log_ratio, sample(mi_dat$th_c), k = k_nn)
)
mi_p <- (sum(mi_null >= mi_obs) + 1) / (length(mi_null) + 1)

# Sensitivity of the estimate to k
mi_by_k <- tibble(k = 2:8) |> 
  mutate(mi = map_dbl(k, \(kk) FNN::mutinfo(mi_dat$log_ratio, mi_dat$th_c, k = kk)))

# Gaussian-equivalent MI from Pearson r, for reference
r_pearson <- cor(mi_dat$log_ratio, mi_dat$th_c)
mi_gauss  <- -0.5 * log(1 - r_pearson^2)

mi_summary <- tibble(
  n         = nrow(mi_dat),
  k         = k_nn,
  mi_nats   = mi_obs,
  mi_bits   = mi_obs / log(2),
  p_perm    = mi_p,
  r_pearson = r_pearson,
  mi_gauss  = mi_gauss
)
mi_summary
mi_by_k

### mutual information within evolution groups: all drugs, th_c and tmax
# Centre within each evolution group (removes group mean differences only).
# KSG uses the max-norm, so the centred variables are then divided by a single
# SD per drug; this is a common rescaling and keeps group differences in spread
TRAITS <- c("th_c", "tmax")
K_GRID <- 2:8
N_PERM <- 2000

traits_c <- all_traits |> 
  group_by(drug, evolution_history.x) |> 
  mutate(across(c(log_ratio, all_of(TRAITS)), \(v) v - mean(v), .names = "{.col}_c")) |> 
  group_by(drug) |> 
  mutate(across(ends_with("_c"), \(v) v / sd(v), .names = "{.col}s")) |> 
  ungroup()

mi_within_group <- function(dat, trait, k) {
  x   <- dat$log_ratio_cs
  y   <- dat[[paste0(trait, "_cs")]]
  grp <- dat$evolution_history.x
  obs <- FNN::mutinfo(x, y, k = k)
  # Permute the trait within groups so the null keeps the group structure
  null <- replicate(N_PERM, FNN::mutinfo(x, ave(y, grp, FUN = sample), k = k))
  tibble(mi = obs, p_perm = (sum(null >= obs) + 1) / (N_PERM + 1))
}

set.seed(5904)
mi_c_all_by_k <- expand_grid(drug = unique(traits_c$drug), trait = TRAITS, k = K_GRID) |> 
  mutate(res = pmap(list(drug, trait, k), \(d, t, kk) 
    mi_within_group(filter(traits_c, drug == d), t, kk))) |> 
  unnest(res)

mi_c_all_by_k |> 
  pivot_wider(names_from = k, values_from = c(mi, p_perm), names_glue = "{.value}_k{k}")

# Within-group Spearman correlations for reference
traits_c |> 
  pivot_longer(all_of(TRAITS), names_to = "trait", values_to = "value") |> 
  group_by(drug, trait, evolution_history.x) |> 
  summarise(n = n(), spearman = cor(log_ratio, value, method = "spearman"), .groups = "drop") |> 
  pivot_wider(names_from = evolution_history.x, values_from = spearman)

traits_c |> 
  pivot_longer(paste0(TRAITS, "_c"), names_to = "trait", values_to = "trait_c") |> 
  mutate(trait = str_remove(trait, "_c$")) |> 
  ggplot(aes(trait_c, log_ratio_c, colour = evolution_history.x)) +
  geom_point(size = 2) +
  facet_grid(drug ~ trait, scales = "free") +
  scale_colour_manual(values = EVO_COLORS, name = "Evolution history") +
  labs(x = "Trait (centred within group)", y = "log ratio (centred within group)")

### variability in resistance: thermal tails vs core
# Tails = populations in the top 15% of |centred trait|, pooled across groups so
# group differences in trait spread are kept (tmax tails are all 40-evolved).
# Spread in resistance = Brown-Forsythe absolute deviation from the group median.
# Resistance spread also differs between groups (e.g. amphotericin), so a naive
# Levene test confounds tail status with evolution history; the permutation
# shuffles trait values within groups to remove that confound.
TAIL_Q <- 0.85

spread_dat <- traits_c |> 
  group_by(drug, evolution_history.x) |> 
  mutate(abs_dev = abs(log_ratio - median(log_ratio))) |> 
  ungroup() |> 
  pivot_longer(paste0(TRAITS, "_c"), names_to = "trait", values_to = "trait_c") |> 
  mutate(trait = str_remove(trait, "_c$"))

tail_stats <- function(abs_dev, trait_c) {
  tail <- abs(trait_c) >= quantile(abs(trait_c), TAIL_Q)
  c(
    ratio    = mean(abs_dev[tail]) / mean(abs_dev[!tail]),
    spearman = cor(abs_dev, abs(trait_c), method = "spearman")
  )
}

set.seed(3362)
spread_tests <- spread_dat |> 
  group_by(drug, trait) |> 
  group_modify(\(d, key) {
    obs  <- tail_stats(d$abs_dev, d$trait_c)
    null <- replicate(N_PERM, tail_stats(
      d$abs_dev, ave(d$trait_c, d$evolution_history.x, FUN = sample)))
    tail <- abs(d$trait_c) >= quantile(abs(d$trait_c), TAIL_Q)
    tibble(
      n_tail           = sum(tail),
      n_tail_40        = sum(tail & d$evolution_history.x == "40 evolved"),
      dev_ratio        = obs[["ratio"]],
      p_ratio_perm     = (sum(null["ratio", ] >= obs[["ratio"]]) + 1) / (N_PERM + 1),
      spearman_abs     = obs[["spearman"]],
      p_spearman_perm  = (sum(null["spearman", ] >= obs[["spearman"]]) + 1) / (N_PERM + 1),
      # Naive Brown-Forsythe test ignoring evolution history, for comparison only
      p_levene_naive   = car::leveneTest(
        d$log_ratio_c, factor(tail), center = median)[["Pr(>F)"]][1]
    )
  }) |> 
  ungroup() |> 
  mutate(p_ratio_bh = p.adjust(p_ratio_perm, method = "BH"))
spread_tests

spread_dat |> 
  group_by(drug, trait) |> 
  mutate(region = if_else(abs(trait_c) >= quantile(abs(trait_c), TAIL_Q), "tail", "core")) |> 
  ungroup() |> 
  ggplot(aes(region, abs_dev)) +
  geom_jitter(aes(colour = evolution_history.x), width = 0.15, height = 0, size = 2) +
  facet_grid(drug ~ trait, scales = "free_y") +
  scale_colour_manual(values = EVO_COLORS, name = "Evolution history") +
  labs(x = NULL, y = "|log ratio - group median|")

### leave-one-out influence on MI: amphotericin-tmax and fluconazole-th_c
# Preprocessing (within-group centring + shared SD) is redone after each removal
# so the dropped population doesn't influence the scaling. k = 3-7 is the range
# where both signals held in the full data.
LOO_PAIRS <- tibble(drug = c("amphotericin", "fluconazole"), trait = c("tmax", "th_c"))
LOO_K     <- 3:7

prep_xy <- function(dat, trait) {
  dat |> 
    group_by(evolution_history.x) |> 
    mutate(x = log_ratio - mean(log_ratio), y = .data[[trait]] - mean(.data[[trait]])) |> 
    ungroup() |> 
    mutate(x = x / sd(x), y = y / sd(y))
}

mi_k <- function(dat, trait, k) {
  d <- prep_xy(dat, trait)
  FNN::mutinfo(d$x, d$y, k = k)
}

loo_mi <- LOO_PAIRS |> 
  mutate(res = map2(drug, trait, \(dr, tr) {
    dat <- filter(all_traits, drug == dr)
    full <- map_dbl(LOO_K, \(kk) mi_k(dat, tr, kk))
    map_dfr(dat$population, \(pop) {
      mi_drop <- map_dbl(LOO_K, \(kk) mi_k(filter(dat, population != pop), tr, kk))
      tibble(
        population = pop,
        evolution_history = dat$evolution_history.x[dat$population == pop],
        mi_full = mean(full),
        mi_drop = mean(mi_drop),
        # Negative = MI falls when this population is removed
        delta   = mean(mi_drop - full)
      )
    })
  })) |> 
  unnest(res)

loo_mi |> 
  group_by(drug, trait) |> 
  slice_min(delta, n = 5) |> 
  ungroup()

# Permutation p (k = 4) after removing the 1, 2, 3 most influential populations
mi_perm_p <- function(dat, trait, k = 4, n_perm = N_PERM) {
  d <- prep_xy(dat, trait)
  obs <- FNN::mutinfo(d$x, d$y, k = k)
  null <- replicate(n_perm, FNN::mutinfo(d$x, ave(d$y, d$evolution_history.x, FUN = sample), k = k))
  tibble(mi = obs, p_perm = (sum(null >= obs) + 1) / (n_perm + 1))
}

set.seed(6158)
loo_drop_top <- LOO_PAIRS |> 
  mutate(res = map2(drug, trait, \(dr, tr) {
    dat <- filter(all_traits, drug == dr)
    top <- loo_mi |> 
      filter(drug == dr, trait == tr) |> 
      arrange(delta) |> 
      pull(population)
    map_dfr(0:3, \(n_drop) {
      dropped <- head(top, n_drop)
      mi_perm_p(filter(dat, !population %in% dropped), tr) |> 
        mutate(n_dropped = n_drop, dropped = paste(dropped, collapse = ", "))
    })
  })) |> 
  unnest(res)
loo_drop_top

# Order populations within each panel separately
loo_mi |> 
  mutate(
    panel = paste(drug, trait, sep = " - "),
    pop_panel = fct_reorder(paste(population, panel, sep = "___"), delta)
  ) |> 
  ggplot(aes(delta, pop_panel, colour = evolution_history)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point(size = 2) +
  facet_wrap(~ panel, scales = "free_y") +
  scale_y_discrete(labels = \(l) str_remove(l, "___.*$")) +
  scale_colour_manual(values = EVO_COLORS, name = "Evolution history") +
  labs(x = "Change in MI when removed (nats, mean over k = 3-7)", y = NULL)
                        