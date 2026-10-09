## Protein Figure: UBF (left column) and c-Myc high peak (right column), side by side

library(tidyverse)
library(patchwork)

# --- Load saved data (written by the "Primary results (Run A)" chunk) ---
pred_ubf      <- readRDS("data/data-gen/pred_ubf.rds")
contrast_ubf  <- readRDS("data/data-gen/contrast_ubf.rds")

pred_cmyc_high     <- readRDS("data/data-gen/pred_cmyc_high.rds")
contrast_cmyc_high <- readRDS("data/data-gen/contrast_cmyc_high.rds")

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
# An asterisk marks a group whose pre -> week 3 change has a 95% interval excluding zero

credible_ubf <- contrast_ubf |>
  filter(hypothesis %in% paste0(tx_names, "_delta"), credible == "Yes") |>
  transmute(tx = str_remove(hypothesis, "_delta$"), time = "w3")

credible_cmyc <- contrast_cmyc_high |>
  filter(hypothesis %in% paste0(tx_names, "_delta"), credible == "Yes") |>
  transmute(tx = str_remove(hypothesis, "_delta$"), time = "w3")


# --- Reusable builder functions ---

# Upper panel: group means at pre and week 3, 95% interval, sexes averaged with equal weight
make_left_panel <- function(pred_dat, ylab, time_levels, credible_dat = NULL, title = NULL) {
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
    scale_x_discrete(labels = c(pre = "Pre", w3 = "Week 3")) +
    labs(x = "", y = ylab, title = title) +
    theme_classic() +
    theme(plot.title = element_text(face = "bold", size = 11))
}

# Lower panel: between-group contrasts as ratios of the pre -> week 3 changes
make_right_panel <- function(contrast_dat) {
  contrast_dat |>
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
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
    labs(x = "Ratio of changes (1 = no difference)", y = NULL) +
    theme_classic() +
    theme(axis.text.y = element_text(size = 9))
}


# --- Build panels ---

p_left_ubf   <- make_left_panel(pred_ubf, "UBF (corrected area)", c("pre", "w3"), credible_ubf,
                                title = "UBF")
p_right_ubf  <- make_right_panel(contrast_ubf)

p_left_cmyc  <- make_left_panel(pred_cmyc_high, "c-Myc, high peak\n(corrected area)", c("pre", "w3"),
                                credible_cmyc, title = "c-Myc (high peak)")
p_right_cmyc <- make_right_panel(contrast_cmyc_high)


# --- Combine: one column per protein (group means on top, contrasts below) ---
# Plot order A, B, C, D matches the tags: A/B = UBF, C/D = c-Myc

p_protein <- wrap_plots(
  list(p_left_ubf, p_right_ubf, p_left_cmyc, p_right_cmyc),
  design  = "AC\nBD",
  heights = c(1, 1.15)
) +
  plot_annotation(tag_levels = "A")

p_protein

ggsave("figures/fig-protein.png", p_protein,
       width = 14, height = 7.5, dpi = 300)

saveRDS(p_protein, "figures/fig-protein.RDS")
