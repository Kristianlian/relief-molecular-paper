## Bicep Curl MVC Figure

#| label: fig-bicep-curl-mvc
#| fig-width: 10
#| fig-height: 7

library(tidyverse)
library(patchwork)

# --- Load saved data ---
pred_bc     <- readRDS("data/data-gen/pred_bc.rds")
contrast_bc <- readRDS("data/data-gen/contrast_bc.rds")


# --- LEFT PANEL: equal-weighted change curve, faceted by tx ---

pred_equal_weighted <- pred_bc |>
  summarise(.by = c(tx, time),
            m     = mean(m),
            lwr50 = mean(lwr50), upr50 = mean(upr50),
            lwr90 = mean(lwr90), upr90 = mean(upr90),
            lwr95 = mean(lwr95), upr95 = mean(upr95)) |>
  mutate(time = factor(time, levels = c("pre", "mid", "post")))

# --- Credible-change markers ---

tx_names <- c("int_old_low", "int_old_mod", "int_yng_low", "int_yng_mod", "con_old_con")

credible_marks <- contrast_bc |>
  filter(hypothesis %in% paste0(rep(tx_names, each = 2), "_delta_", c("mid", "post")),
         credible == "Yes") |>
  mutate(tx   = str_remove(hypothesis, "_delta_(mid|post)$"),
         time = str_extract(hypothesis, "(mid|post)$"),
         time = factor(time, levels = c("pre", "mid", "post"))) |>
  select(tx, time)

asterisk_offset <- diff(range(pred_equal_weighted$upr95, na.rm = TRUE)) * 0.06

asterisk_data <- pred_equal_weighted |>
  inner_join(credible_marks, by = c("tx", "time")) |>
  mutate(y_star = upr95 + asterisk_offset)

p_left <- pred_equal_weighted |>
  ggplot(aes(time, m, group = tx)) +
  geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0.1, color = "steelblue") +
  geom_line(linewidth = 0.8, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  geom_text(data = asterisk_data, aes(x = time, y = y_star, label = "*"),
            inherit.aes = FALSE, size = 5, color = "black", vjust = 0) +
  facet_wrap(~ tx, ncol = 5,
             labeller = labeller(tx = c(
               int_old_low = "Old, Low",
               int_old_mod = "Old, Mod",
               int_yng_low = "Young, Low",
               int_yng_mod = "Young, Mod",
               con_old_con = "Old, Control"
             ))) +
  labs(x = "", y = "Bicep curl MVC (N)") +
  theme_classic()


# --- RIGHT PANEL: 5 key comparisons, at mid and post ---

key_base <- c("old_vs_con_delta", "yng_vs_old_delta",
              "yng_mod_vs_low", "old_mod_vs_low", "yng_vs_old_vol")

base_labels <- c(
  yng_vs_old_delta = "Young vs. Old\n(intervention)",
  old_vs_con_delta = "Old: Intervention\nvs. Control",
  yng_mod_vs_low   = "Young: Mod\nvs. Low",
  old_mod_vs_low   = "Old: Mod\nvs. Low",
  yng_vs_old_vol   = "Volume effect:\nYoung vs. Old"
)

plot_data <- contrast_bc |>
  filter(hypothesis %in% paste0(rep(key_base, each = 2), "_", c("mid", "post"))) |>
  mutate(
    base = str_remove(hypothesis, "_(mid|post)$"),
    tp   = str_extract(hypothesis, "(mid|post)$"),
    tp   = factor(tp, levels = c("post", "mid"), labels = c("Pre-Post", "Pre-Mid")),
    base = factor(base, levels = rev(key_base)),
    row_label = paste(base, tp, sep = " | "),
    pos_label = paste0(round(pos * 100), "%")
  ) |>
  arrange(base, desc(tp)) |>
  mutate(row_label = factor(row_label, levels = unique(row_label))) |>
  mutate(row_label_display = if_else(tp == "Pre-Post",
                                     as.character(base_labels[as.character(base)]),
                                     ""))

p_right <- plot_data |>
  ggplot(aes(y = row_label)) +
  geom_vline(xintercept = 1, linetype = 2, color = "grey40") +
  geom_pointrange(aes(x = mean, xmin = lower95, xmax = upper95, color = tp),
                  linewidth = 0.8, size = 0.5) +
  geom_text(aes(x = upper95, label = pos_label),
            hjust = -0.2, size = 3, color = "grey30") +
  scale_y_discrete(labels = setNames(plot_data$row_label_display, plot_data$row_label)) +
  scale_color_manual(values = c("Pre-Mid" = "grey60", "Pre-Post" = "steelblue"),
                     name = "Timepoint") +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(x = "Ratio (post/pre)", y = NULL) +
  theme_classic() +
  theme(axis.text.y = element_text(size = 8),
        legend.position = "bottom")


# --- COMBINE ---

p_bc <- p_left + p_right +
  plot_layout(widths = c(2, 1), guides = "collect") &
  theme(legend.position = "bottom")

p_bc

saveRDS(p_bc, "figures/fig-bicep-curl.RDS")

ggsave("figures/fig-bicep-curl.png", p_bc, width = 10, height = 7, dpi = 300)
