## Muscle volume figures

library(tidyverse)
library(cowplot)
library(ggdist)
library(patchwork)

# ── Shared aesthetics ────────────────────────────────────────────────────────
col_yng <- "#2166ac"

base_theme <- theme_minimal(base_size = 8) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.3),
    axis.ticks = element_line(colour = "black", linewidth = 0.3),
    legend.position = "none",
    strip.text = element_text(size = 7, face = "bold"),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6)
  )

# ── Read data ────────────────────────────────────────────────────────────────
contrast_mv_draws <- readRDS("data/data-gen/contrast_mv_draws.rds")
contrast_mv       <- readRDS("data/data-gen/contrast_mv.rds")
pred_mv           <- readRDS("data/data-gen/pred_mv.rds")

# ── Panel A: trajectory left panel, same style as thickness/lean mass ──────

tx_labels <- c(
  int_old_low = "Old, Low",
  int_old_mod = "Old, Mod",
  int_yng_low = "Young, Low",
  int_yng_mod = "Young, Mod",
  con_old_con = "Old, Control"
)

pred_equal_weighted <- pred_mv |>
  summarise(.by = c(tx, time),
            m     = mean(m),
            lwr50 = mean(lwr50), upr50 = mean(upr50),
            lwr90 = mean(lwr90), upr90 = mean(upr90),
            lwr95 = mean(lwr95), upr95 = mean(upr95)) |>
  mutate(time = factor(time, levels = c("pre", "post")))

# --- Credible-change markers ---

tx_names <- names(tx_labels)

credible_marks <- contrast_mv |>
  filter(hypothesis %in% paste0(tx_names, "_delta"), credible == "Yes") |>
  transmute(tx = str_remove(hypothesis, "_delta$"), time = "post")

asterisk_offset <- diff(range(pred_equal_weighted$upr95, na.rm = TRUE)) * 0.06

asterisk_data <- pred_equal_weighted |>
  inner_join(credible_marks |> mutate(time = factor(time, levels = c("pre", "post"))),
             by = c("tx", "time")) |>
  mutate(y_star = upr95 + asterisk_offset)

p_left <- pred_equal_weighted |>
  ggplot(aes(time, m, group = tx)) +
  geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0.1, color = "steelblue") +
  geom_line(linewidth = 0.8, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  geom_text(data = asterisk_data, aes(x = time, y = y_star, label = "*"),
            inherit.aes = FALSE, size = 5, color = "black", vjust = 0) +
  facet_wrap(~ tx, ncol = 5, labeller = labeller(tx = tx_labels)) +
  labs(x = "", y = "Muscle volume (cm\u00b3)") +
  theme_classic()

# ── Panel B: half-eye density + point + CI, ordered as specified ───────────
contrast_labels <- c(
  old_vs_con_delta  = "Intervention vs. Control",
  yng_vs_old_delta  = "Young vs. Old",
  yng_mod_vs_low    = "Mod vs. Low (Young)",
  old_mod_vs_low    = "Mod vs. Low (Old)",
  yng_vs_old_vol    = "Volume effect: Young vs. Old"
)

contrast_order <- c(
  "Intervention vs. Control",
  "Young vs. Old",
  "Mod vs. Low (Young)",
  "Mod vs. Low (Old)",
  "Volume effect: Young vs. Old"
)

contrast_mv_draws <- contrast_mv_draws %>%
  mutate(
    label = contrast_labels[hypothesis],
    label = factor(label, levels = rev(contrast_order))
  )

# pos% labels, positioned at the right edge of each row's density
pos_labels <- contrast_mv %>%
  filter(hypothesis %in% names(contrast_labels)) %>%
  mutate(
    label = contrast_labels[hypothesis],
    label = factor(label, levels = rev(contrast_order)),
    pos_pct = paste0(round(pos * 100), "%")
  )

# x-position for the label: just past each row's own upper95, so it doesn't
# overlap the density regardless of how wide each distribution is
pos_labels <- pos_labels %>%
  mutate(x_pos = upper95 + 0.08 * diff(range(contrast_mv_draws$draw)))  # was 0.01

fig_muscle_volume <- ggplot(contrast_mv_draws, aes(x = draw, y = label)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.3) +
  stat_halfeye(
    aes(fill = after_stat(x > 1)),
    .width = c(0.5, 0.95),
    point_interval = "mean_qi",
    height = 0.7,
    point_size = 1.8,
    interval_size = 1,
    slab_alpha = 0.6
  ) +
  geom_text(
    data = pos_labels,
    aes(x = x_pos, y = label, label = pos_pct),
    inherit.aes = FALSE,
    hjust = 0, size = 2.5
  ) +
  scale_fill_manual(values = c(`TRUE` = col_yng, `FALSE` = "grey60"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.25))) +  # was 0.15, more right-side room
  labs(x = "Ratio (post/pre)", y = NULL) +
  base_theme +
  theme(axis.text.y = element_text(hjust = 0, size = 7))

saveRDS(fig_muscle_volume, "./figures/fig-muscle-volume-density.RDS")

ggsave(
  filename = "./figures/fig-muscle-volume-density.png",
  plot = fig_muscle_volume,
  device = "png",
  width = 140,
  height = 90,
  units = "mm",
  dpi = 300,
  bg = "white"
)

# ── Combined figure: trajectory (left) + density contrasts (right) ─────────

fig_muscle_volume_combined <- p_left + fig_muscle_volume + plot_layout(widths = c(2, 1.3))

fig_muscle_volume_combined

saveRDS(fig_muscle_volume_combined, "./figures/fig-muscle-volume-combined.RDS")

ggsave(
  filename = "./figures/fig-muscle-volume-combined.png",
  plot = fig_muscle_volume_combined,
  device = "png",
  width = 220,
  height = 100,
  units = "mm",
  dpi = 300,
  bg = "white"
)
