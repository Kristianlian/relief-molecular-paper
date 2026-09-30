## HUMAC Figure

#| label: fig-humac
#| fig-width: 11
#| fig-height: 7

library(tidyverse)
library(patchwork)
library(ggrepel)

# --- Load saved data ---
pred_isom  <- readRDS("data/data-gen/pred_humac_isom.rds")  |> mutate(speed = "Isometric")
pred_60    <- readRDS("data/data-gen/pred_humac_60.rds")    |> mutate(speed = "60°/s")
pred_120   <- readRDS("data/data-gen/pred_humac_120.rds")   |> mutate(speed = "120°/s")
pred_240   <- readRDS("data/data-gen/pred_humac_240.rds")   |> mutate(speed = "240°/s")

# results_humac <- readRDS("data/data-gen/results_humac.rds")
results_humac_standard <- readRDS("data/data-gen/results_humac_standard.rds")


# --- Build plot_data_humac from results_humac ----------------------

key_base <- c("yng_vs_old_delta", "old_vs_con_delta",
              "yng_mod_vs_low", "old_mod_vs_low", "yng_vs_old_vol")

contrast_labels_humac <- c(
  yng_vs_old_delta = "Young vs. Old\n(intervention)",
  old_vs_con_delta = "Old: Intervention\nvs. Control",
  yng_mod_vs_low   = "Young: Mod\nvs. Low",
  old_mod_vs_low   = "Old: Mod\nvs. Low",
  yng_vs_old_vol   = "Volume effect:\nYoung vs. Old"
)

plot_data_humac <- results_humac_standard |>
  filter(timepoint == "Pre-post") |>
  mutate(pos_label = paste0(round(pos * 100), "%"),
         group = factor(group, levels = key_base),          
         group_label = contrast_labels_humac[as.character(group)],
         group_label = factor(group_label, levels = contrast_labels_humac[key_base]))


# --- LEFT PANEL: pre/post change, faceted by tx, colored by speed ---

pred_all <- bind_rows(pred_isom, pred_60, pred_120, pred_240) |>
  mutate(speed = factor(speed, levels = c("Isometric", "60°/s", "120°/s", "240°/s")),
         time  = factor(time, levels = c("pre", "post")),
         tx    = factor(tx, levels = c("int_old_low", "int_old_mod", "int_yng_low",
                                       "int_yng_mod", "con_old_con")))

p_left <- pred_all |>
  ggplot(aes(time, m, color = speed, group = speed)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0.08, alpha = 0.5) +
  facet_wrap(~ tx, ncol = 5,
             labeller = labeller(tx = c(
               int_old_low = "Old, Low",
               int_old_mod = "Old, Mod",
               int_yng_low = "Young, Low",
               int_yng_mod = "Young, Mod",
               con_old_con = "Old, Control"
             ))) +
  scale_color_manual(values = c("Isometric" = "#08306b", "60°/s" = "#4292c6",
                                "120°/s" = "#9ecae1", "240°/s" = "#c6dbef"),
                     name = "Speed") +
  labs(x = "", y = "Peak torque (Nm)") +
  theme_classic()


# --- RIGHT PANEL ---

p_right_nolegend <- plot_data_humac |>
  mutate(speed_label = factor(speed_label, levels = c("Isometric", "60°/s", "120°/s", "240°/s")),
         label_x = if_else(mean >= 0, upper95, lower95),
         label_hjust = if_else(mean >= 0, -0.2, 1.2)) |>
  ggplot(aes(y = speed_label, color = speed_label)) +
  geom_vline(xintercept = 0, linetype = 2, color = "grey40") +
  geom_pointrange(aes(x = mean, xmin = lower95, xmax = upper95),
                  linewidth = 0.6, size = 0.35) +
  geom_text_repel(aes(x = label_x, label = pos_label, hjust = label_hjust),
                  size = 2.8, color = "grey20",
                  direction = "y",
                  seed = 1,
                  segment.color = NA,    
                  box.padding = 0.15,
                  nudge_x = if_else(plot_data_humac$mean >= 0, 3, -3)) +
  facet_wrap(~ group_label, ncol = 1, strip.position = "left") +
  scale_color_manual(values = c("Isometric" = "#08306b", "60°/s" = "#4292c6",
                                "120°/s" = "#9ecae1", "240°/s" = "#c6dbef"),
                     name = "Speed") +
  scale_x_continuous(expand = expansion(mult = c(0.15, 0.3))) +
  labs(x = "Δ Peak torque (Nm), post-pre", y = NULL) +
  theme_classic() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, size = 8, hjust = 1),
        strip.background = element_blank(),
        legend.position = "none")


# --- COMBINE with a single shared legend ---

library(cowplot)

p_left_nolegend <- p_left + theme(legend.position = "none")

legend_shared <- cowplot::get_legend(
  p_left + theme(legend.position = "top", legend.title = element_text(size = 10))
)

p_body <- p_left_nolegend / p_right_nolegend + plot_layout(heights = c(1, 1.6))

p_humac <- plot_grid(legend_shared, p_body, ncol = 1, rel_heights = c(0.05, 1))

p_humac

ggsave("figures/fig3-humac.png", p_humac, width = 11, height = 12, dpi = 300)






## --- OLD FIG -------------------------------------------------------
#
## --- Build plot_data_humac from results_humac ----------------------
#
#contrast_labels_humac <- c(
#  "Volume (older)"   = "Old: Mod\nvs. Low",
#  "Volume (younger)" = "Young: Mod\nvs. Low",
#  "Intervention vs control (low, older)" = "Old: Low vs.\nControl",
#  "Intervention vs control (mod, older)" = "Old: Mod vs.\nControl",
#  "Age (low volume)" = "Old vs. Young\n(Low volume)",
#  "Age (mod volume)" = "Old vs. Young\n(Mod volume)"
#)
#
#plot_data_humac <- results_humac |>
#  filter(comparison == "Between-group",
#         timepoint == "Pre-post") |>
#  mutate(pos_label = paste0(round(pos * 100), "%"),
#         group_label = contrast_labels_humac[group])
#
## --- LEFT PANEL: pre/post change, faceted by tx, colored by speed ---
#
#pred_all <- bind_rows(pred_isom, pred_60, pred_120, pred_240) |>
#  mutate(speed = factor(speed, levels = c("Isometric", "60°/s", "120°/s", "240°/s")),
#         time  = factor(time, levels = c("pre", "post")),
#         tx    = factor(tx, levels = c("int_old_low", "int_old_mod", "int_yng_low",
#                                       "int_yng_mod", "con_old_con")))
#
#p_left <- pred_all |>
#  ggplot(aes(time, m, color = speed, group = speed)) +
#  geom_line(linewidth = 0.8) +
#  geom_point(size = 2) +
#  geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0.08, alpha = 0.5) +
#  facet_wrap(~ tx, ncol = 5,
#             labeller = labeller(tx = c(
#               int_old_low = "Old, Low",
#               int_old_mod = "Old, Mod",
#               int_yng_low = "Young, Low",
#               int_yng_mod = "Young, Mod",
#               con_old_con = "Old, Control"
#             ))) +
#  scale_color_manual(values = c("Isometric" = "#08306b", "60°/s" = "#4292c6",
#                                "120°/s" = "#9ecae1", "240°/s" = "#c6dbef"),
#                     name = "Speed") +
#  labs(x = "", y = "Peak torque (Nm)") +
#  theme_classic()
#
#
## --- RIGHT PANEL ---
#
#p_right_nolegend <- plot_data_humac |>
#  mutate(speed_label = factor(speed_label, levels = c("Isometric", "60°/s", "120°/s", "240°/s")),
#         label_x = if_else(mean >= 0, upper95, lower95),
#         label_hjust = if_else(mean >= 0, -0.2, 1.2)) |>
#  ggplot(aes(y = speed_label, color = speed_label)) +
#  geom_vline(xintercept = 0, linetype = 2, color = "grey40") +
#  geom_pointrange(aes(x = mean, xmin = lower95, xmax = upper95),
#                  linewidth = 0.6, size = 0.35) +
#  geom_text(aes(x = label_x, label = pos_label, hjust = label_hjust),
#            size = 2.8, color = "grey20") +
#  facet_wrap(~ group_label, ncol = 1, strip.position = "left") +
#  scale_color_manual(values = c("Isometric" = "#08306b", "60°/s" = "#4292c6",
#                                "120°/s" = "#9ecae1", "240°/s" = "#c6dbef"),
#                     name = "Speed") +
#  scale_x_continuous(expand = expansion(mult = c(0.15, 0.3))) +
#  labs(x = "Δ Peak torque (Nm), post-pre", y = NULL) +
#  theme_classic() +
#  theme(axis.text.y = element_blank(),
#        axis.ticks.y = element_blank(),
#        strip.placement = "outside",
#        strip.text.y.left = element_text(angle = 0, size = 8, hjust = 1),
#        strip.background = element_blank(),
#        legend.position = "none")
#
#
## --- COMBINE with a single shared legend ---
#
#library(cowplot)
#
#p_left_nolegend <- p_left + theme(legend.position = "none")
#
#legend_shared <- cowplot::get_legend(
#  p_left + theme(legend.position = "top", legend.title = element_text(size = 10))
#)
#
#p_body <- p_left_nolegend / p_right_nolegend + plot_layout(heights = c(1, 1.6))
#
#p_humac <- plot_grid(legend_shared, p_body, ncol = 1, rel_heights = c(0.05, 1))
#
#p_humac
#
#ggsave("figures/archive/fig5-humac-alt.png", p_humac, width = 11, height = 12, dpi = 300)
#
