## Total RNA figure

library(tidyverse)
library(patchwork)


# --- Load saved data ---

pred_rna              <- readRDS("data/data-gen/pred_rna.rds")
contrast_rna          <- readRDS("data/data-gen/contrast_rna.rds")
contrast_summary      <- readRDS("data/data-gen/rna_mg_contrast_summary_m5.rds")

contrast_summary |>
  mutate(across(c(fc_est, fc_lower, fc_upper), ~ round((.x - 1) * 100, 2)),
         pos = round(pos, 4)) |>
  select(contrast, fc_est, fc_lower, fc_upper, pos) |>
  print(n = Inf, width = Inf)


# --- Shared contrast setup ---

key_contrasts <- c("old_vs_con_delta", "yng_vs_old_delta",
                   "yng_mod_vs_low", "old_mod_vs_low", "yng_vs_old_vol")

contrast_labels <- c(
  yng_vs_old_delta = "Young vs. Old\n(intervention)",
  old_vs_con_delta = "Old: Intervention\nvs. Control",
  yng_mod_vs_low   = "Young: Mod\nvs. Low",
  old_mod_vs_low   = "Old: Mod\nvs. Low",
  yng_vs_old_vol   = "Volume effect:\nYoung vs. Old"
)

tx_labels <- c(
  int_old_low = "Old, Low",
  int_old_mod = "Old, Mod",
  int_yng_low = "Young, Low",
  int_yng_mod = "Young, Mod",
  con_old_con = "Old, Control"
)
tx_names <- names(tx_labels)


# --- Credible-change markers, per outcome ---

credible_rna <- contrast_summary |>
  filter(contrast %in% paste0(rep(tx_names, each = 2), "_", c("w3", "post"))) |>
  mutate(tx   = str_remove(contrast, "_(w3|post)$"),
         time = str_extract(contrast, "(w3|post)$"),
         credible = fc_lower > 1 | fc_upper < 1) |>
  filter(credible) |>
  select(tx, time)


# --- Reusable builder functions ---

make_left_panel <- function(pred_dat, ylab, time_levels, credible_dat = NULL) {
  pred_equal_weighted <- pred_dat |>
    summarise(.by = c(tx, time),
              m     = mean(m),
              lwr50 = mean(lwr50), upr50 = mean(upr50),
              lwr90 = mean(lwr90), upr90 = mean(upr90),
              lwr95 = mean(lwr95), upr95 = mean(upr95)) |>
    mutate(time = factor(time, levels = time_levels))
  
  p <- pred_equal_weighted |>
    ggplot(aes(time, m, group = tx)) +
    geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0.1, color = "steelblue") +
    geom_line(linewidth = 0.8, color = "steelblue") +
    geom_point(size = 2, color = "steelblue")
  
  if (!is.null(credible_dat)) {
    asterisk_offset <- diff(range(pred_equal_weighted$upr95, na.rm = TRUE)) * 0.06
    asterisk_data <- pred_equal_weighted |>
      inner_join(credible_dat |> mutate(time = factor(time, levels = time_levels)),
                 by = c("tx", "time")) |>
      mutate(y_star = upr95 + asterisk_offset)
    
    p <- p + geom_text(data = asterisk_data, aes(x = time, y = y_star, label = "*"),
                       inherit.aes = FALSE, size = 5, color = "black", vjust = 0)
  }
  
  p +
    facet_wrap(~ tx, ncol = 5, labeller = labeller(tx = tx_labels)) +
    labs(x = "", y = ylab) +
    theme_classic()
}

# Dual-delta right panel (RNA — pre -> w3 AND pre -> post both shown)
make_right_panel_dual <- function(contrast_dat) {
  plot_data <- contrast_dat |>
    filter(hypothesis %in% key_contrasts) |>
    mutate(
      tp   = factor(delta, levels = c("pre to post", "pre to w3"),
                    labels = c("Pre-Post", "Pre-W3")),
      hypothesis = factor(hypothesis, levels = rev(key_contrasts)),
      row_label  = paste(hypothesis, tp, sep = " | "),
      pos_label  = paste0(round(pos * 100), "%")
    ) |>
    arrange(hypothesis, desc(tp)) |>
    mutate(row_label = factor(row_label, levels = unique(row_label))) |>
    mutate(row_label_display = if_else(tp == "Pre-Post",
                                       as.character(contrast_labels[as.character(hypothesis)]),
                                       ""))
  
  plot_data |>
    ggplot(aes(y = row_label)) +
    geom_vline(xintercept = 1, linetype = 2, color = "grey40") +
    geom_pointrange(aes(x = mean, xmin = lower95, xmax = upper95, color = tp),
                    linewidth = 0.8, size = 0.5) +
    geom_text(aes(x = upper95, label = pos_label),
              hjust = -0.2, size = 3, color = "grey30") +
    scale_y_discrete(labels = setNames(plot_data$row_label_display, plot_data$row_label)) +
    scale_color_manual(values = c("Pre-W3" = "grey60", "Pre-Post" = "steelblue"),
                       name = "Timepoint") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(x = "Ratio (post/pre)", y = NULL) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 8),
          legend.position = "top")
}


# --- Build panels ---

p_left_rna   <- make_left_panel(pred_rna, "Total RNA (ng)", c("pre", "w3", "post"), credible_rna)
p_right_rna  <- make_right_panel_dual(contrast_rna)

p_rna <- (p_left_rna  + p_right_rna  + plot_layout(widths = c(2, 1)))

p_rna

ggsave("figures/fig-rna.png", p_rna,
       width = 10, height = 6, dpi = 300)

saveRDS(p_rna, "figures/fig-rna.RDS")
