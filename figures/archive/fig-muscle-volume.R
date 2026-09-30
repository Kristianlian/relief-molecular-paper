## Muscle Volume figure

#| label: fig-muscle-volume
#| fig-width: 10
#| fig-height: 6

library(tidyverse)
library(patchwork)

# --- Load saved data ---
pred_mv     <- readRDS("data/data-gen/pred_mv.rds")
contrast_mv <- readRDS("data/data-gen/contrast_mv.rds")


# --- LEFT PANEL: equal-weighted change curve, faceted by tx ---
# NOTE: pred_mv is saved split by sex (tx, sex, time). This averages the
# already-summarized per-sex means/quantiles as a pragmatic equal-weight
# approximation, not a full re-derivation from posterior draws. Revisit
# once the underlying .qmd is updated to save a properly equal-weighted
# pred_mv directly.

pred_equal_weighted <- pred_mv |>
  summarise(.by = c(tx, time),
            m     = mean(m),
            lwr50 = mean(lwr50), upr50 = mean(upr50),
            lwr90 = mean(lwr90), upr90 = mean(upr90),
            lwr95 = mean(lwr95), upr95 = mean(upr95))

p_left <- pred_equal_weighted |>
  ggplot(aes(time, m, group = tx)) +
  geom_ribbon(aes(ymin = lwr90, ymax = upr90), alpha = 0.2, fill = "steelblue") +
  geom_ribbon(aes(ymin = lwr50, ymax = upr50), alpha = 0.35, fill = "steelblue") +
  geom_line(linewidth = 0.8, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  facet_wrap(~ tx, ncol = 5,
             labeller = labeller(tx = c(
               int_old_low = "Old, Low",
               int_old_mod = "Old, Mod",
               int_yng_low = "Young, Low",
               int_yng_mod = "Young, Mod",
               con_old_con = "Old, Control"
             ))) +
  labs(x = "", y = "Muscle volume (cm³)") +
  theme_classic()


# --- RIGHT PANEL: the 5 key comparison contrasts only ---
# NOTE: contrast_mv is derived from m2_preds$contr (equal-weighted at the
# posterior-draw level already, since m2_pred_contr() collapses sex before
# summarising) — so this panel does not have the same approximation issue
# as the left panel.

key_contrasts <- c("yng_vs_old_delta", "old_vs_con_delta",
                   "yng_mod_vs_low", "old_mod_vs_low", "yng_vs_old_vol")

contrast_labels <- c(
  yng_vs_old_delta = "Young vs. Old\n(intervention)",
  old_vs_con_delta = "Old: Intervention\nvs. Control",
  yng_mod_vs_low   = "Young: Mod\nvs. Low",
  old_mod_vs_low   = "Old: Mod\nvs. Low",
  yng_vs_old_vol   = "Volume effect:\nYoung vs. Old"
)

# contrast_mv only has summarised mean/lower95/upper95, not raw draws,
# so we can't redraw a density curve from it directly — use a point +
# interval representation instead (still shows the same information,
# just not as a smooth density). Go back and save the raw draws if we want 
# the actual density curve

p_right <- contrast_mv |>
  filter(hypothesis %in% key_contrasts) |>
  mutate(hypothesis = factor(hypothesis, levels = rev(key_contrasts)),
         pos_label = paste0(round(pos * 100), "%")) |>
  ggplot(aes(y = hypothesis)) +
  geom_vline(xintercept = 1, linetype = 2, color = "grey40") +
  geom_pointrange(aes(x = mean, xmin = lower95, xmax = upper95),
                  color = "steelblue", linewidth = 0.8, size = 0.6) +
  geom_text(aes(x = upper95, label = pos_label),
            hjust = -0.2, size = 3, color = "grey30") +
  scale_y_discrete(labels = contrast_labels) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +  # room for the label
  labs(x = "Ratio (post/pre)", y = NULL) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 9))


# --- COMBINE ---

p_volume <- p_left + p_right + plot_layout(widths = c(2, 1))
p_volume

#ggsave("figures/fig2-muscle-volume.png", p_volume, width = 10, height = 6, dpi = 300)


