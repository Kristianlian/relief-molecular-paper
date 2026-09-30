# OGTT figure
# Standalone figure generation for the pre/post OGTT trajectory
# and the young-vs-old training-response comparison.

library(tidyverse)
library(brms)
library(splines)
library(marginaleffects)
library(reliefdata)
library(patchwork)

# --- Rebuild the data (needed for the scale/center attributes used
# to back-transform predictions to the original glucose scale) ---

ogtt_dat <- reliefdata::relief_glucose %>%
  group_by(participant, time) %>%
  inner_join(relief_participants %>% mutate(participant = as.numeric(participant))) %>%
  mutate(gruppe = paste(age.grp, allocation, sep = "_"),
         time = factor(time, levels = c("pre", "post"))) %>%
  filter(!is.na(glucose)) %>%
  ungroup() %>%
  mutate(stglucose = scale(glucose),
         sampletime = if_else(is.na(sampletime), idealtime, sampletime)) %>%
  select(participant, time, gruppe, sampletime, glucose, stglucose) %>%
  data.frame()

scale_  <- attr(ogtt_dat$stglucose, "scaled:scale")
center_ <- attr(ogtt_dat$stglucose, "scaled:center")

# --- Load the cached fitted model (no refit) ---

m3 <- readRDS("fits/01-ogtt/m3.rds")

# --- Predictions across the sampling window, for each group ---

nd <- expand_grid(sampletime = seq(0, 120, 2),
                  grp = c('post_old_con', 'post_old_int', 'post_young_int',
                          'pre_old_con', 'pre_old_int', 'pre_young_int'),
                  participant = 1) %>%
  data.frame()

preds <- predictions(m3, newdata = nd, allow_new_levels = TRUE)

# --- Within-group and between-group comparison draws, back-transformed ---

comp_df <- posterior_draws(preds) %>%
  filter(drawid %in% 1:2000) %>%
  select(drawid, sampletime, draw, grp, participant) %>%
  mutate(draw = draw * scale_ + center_) %>%
  pivot_wider(names_from = grp, values_from = draw) %>%
  mutate(yng = post_young_int - pre_young_int,
         old = post_old_int - pre_old_int,
         con = post_old_con - pre_old_con) %>%
  mutate(yng_vs_old = yng - old,
         yng_vs_con = yng - con,
         old_vs_con = old - con)

saveRDS(comp_df, "data/data-gen/comp_df_ogtt.rds")

# --- Panel A: pre/post trajectories by age group ---

time_df <- comp_df |>
  dplyr::select(drawid:pre_young_int) |>
  pivot_longer(cols = post_old_con:pre_young_int) |>
  separate(name, into = c("time", "age", "grp")) |>
  summarise(.by = c(sampletime, time, age, grp),
            m = mean(value, na.rm = TRUE),
            l95 = quantile(value, 0.025, na.rm = TRUE),
            u95 = quantile(value, 0.975, na.rm = TRUE),
            l80 = quantile(value, 0.1, na.rm = TRUE),
            u80 = quantile(value, 0.9, na.rm = TRUE),
            l50 = quantile(value, 0.25, na.rm = TRUE),
            u50 = quantile(value, 0.75, na.rm = TRUE))

p1_ogtt <- time_df |>
  filter(!(age == "old" & grp == "con")) |>
  mutate(time = factor(time, levels = c("pre", "post"),
                       labels = c("Pre- intervention", "Post-intervention")),
         age = factor(age, levels = c("young", "old"),
                      labels = c("Young (20-30 yrs)", "Old (70+ years)"))) |>
  ggplot(aes(sampletime, m,
             group = paste(time, grp, age),
             color = age)) +
  geom_ribbon(aes(ymin = l50, ymax = u50, fill = age, color = NULL), alpha = 0.4) +
  geom_ribbon(aes(ymin = l80, ymax = u80, fill = age, color = NULL), alpha = 0.2) +
  geom_text(data = data.frame(time = factor("post",
                                            levels = c("pre", "post"),
                                            labels = c("Pre- intervention", "Post-intervention")),
                              x = rep(121, 5),
                              y = c(4.8, 5.15, 5.6, 6.05, 6.45),
                              lab = c("10%", "25%", "Mean", "75%", "90%")),
            aes(x, y, label = lab, group = NULL, color = NULL),
            show.legend = FALSE, size = 2.5, hjust = 0) +
  geom_line() +
  scale_x_continuous(expand = c(0, 15)) +
  facet_wrap(~ time) +
  scale_color_manual(values = c("#0173B2", "#DE8F05")) +
  scale_fill_manual(values = c("#0173B2", "#DE8F05")) +
  labs(x = "Sample time (min)", y = "Blood glucose (mmol L-1)",
       color = "Age group", fill = "Age group",
       title = "Pre- and post-training OGTT in young and old participants") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.position = "bottom")

# --- Panel B: young vs. old training-response comparison ---

diffcom_df <- comp_df |>
  dplyr::select(drawid:sampletime, yng_vs_old) |>
  summarise(.by = sampletime,
            m = mean(yng_vs_old, na.rm = TRUE),
            l95 = quantile(yng_vs_old, 0.025, na.rm = TRUE),
            u95 = quantile(yng_vs_old, 0.975, na.rm = TRUE),
            l80 = quantile(yng_vs_old, 0.1, na.rm = TRUE),
            u80 = quantile(yng_vs_old, 0.9, na.rm = TRUE),
            l50 = quantile(yng_vs_old, 0.25, na.rm = TRUE),
            u50 = quantile(yng_vs_old, 0.75, na.rm = TRUE))

p2_ogtt <- diffcom_df |>
  ggplot(aes(sampletime, m)) +
  geom_hline(yintercept = 0, lty = 2, color = "gray50") +
  geom_ribbon(aes(ymin = l50, ymax = u50), fill = "#009E73", alpha = 0.4) +
  geom_ribbon(aes(ymin = l80, ymax = u80), fill = "#009E73", alpha = 0.2) +
  geom_ribbon(aes(ymin = l95, ymax = u95), fill = "#009E73", alpha = 0.1) +
  geom_line() +
  labs(x = "Sample time (min)", y = "Blood glucose (mmol L-1)",
       subtitle = "Blood glucose difference pre- to post-intervention, Young - Old ") +
  annotate("text",
           x = rep(121, 7),
           y = c(-0.27, -0.07, 0.15, 0.37, 0.6, 0.8, 1.05),
           label = c("2.5%", "10%", "25%", "Mean", "75%", "90%", "97.5%"),
           size = 2.5, hjust = 0) +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        legend.position = "bottom",
        plot.margin = unit(c(0, 60, 0, 60), "pt"))

# --- Combine ---

p_ogtt <- p1_ogtt / p2_ogtt

p_ogtt

saveRDS(p_ogtt, "figures/fig-ogtt.RDS")

ggsave("figures/fig-ogtt.png", p_ogtt,
       width = 180, height = 200, units = "mm", dpi = 300, bg = "white")
 