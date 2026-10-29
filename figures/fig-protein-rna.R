## Combined figure: change from baseline (UBF, c-Myc, total RNA) + total RNA vs. UBF

library(tidyverse)
library(patchwork)

# --- Load saved model output ---
contrast_ubf         <- readRDS("data/data-gen/contrast_ubf.rds")          # Run A
contrast_cmyc_high   <- readRDS("data/data-gen/contrast_cmyc_high.rds")    # Run A
contrast_summary_rna <- readRDS("data/data-gen/rna_mg_contrast_summary_m5.rds")

# Total RNA vs. UBF: paired data and model-predicted lines, both written by the
# "Total RNA vs UBF" section of the protein qmd (model m_corr)
rna_ubf    <- readRDS("data/data-gen/rna_ubf.rds")
corr_lines <- readRDS("data/data-gen/corr_lines_rna_ubf.rds")

# --- Groups ---
tx_names  <- c("int_old_low", "int_old_mod", "int_yng_low", "int_yng_mod", "con_old_con")
tx_labels <- c(int_old_low = "Old\nLow",   int_old_mod = "Old\nMod",
               int_yng_low = "Young\nLow", int_yng_mod = "Young\nMod",
               con_old_con = "Old\nControl")


# ============================================================
# Change from baseline (percent), one row per outcome x group x time
# ============================================================

# Protein: baseline -> week 3 only (UBF and c-Myc were not measured at post)
protein_change <- function(contrast_dat, outcome) {
  contrast_dat |>
    filter(hypothesis %in% paste0(tx_names, "_delta")) |>
    transmute(outcome = outcome,
              tx   = str_remove(hypothesis, "_delta$"),
              time = "w3",
              est = pct_change, lo = pct_lower, hi = pct_upper)
}

# Total RNA (model 5): baseline -> week 3 and baseline -> post
rna_change <- contrast_summary_rna |>
  filter(contrast %in% paste0(rep(tx_names, each = 2), "_", c("w3", "post"))) |>
  transmute(outcome = "Total RNA",
            tx   = str_remove(contrast, "_(w3|post)$"),
            time = str_extract(contrast, "(w3|post)$"),
            est = (fc_est - 1) * 100, lo = (fc_lower - 1) * 100, hi = (fc_upper - 1) * 100)

change_dat <- bind_rows(
  protein_change(contrast_ubf, "UBF"),
  protein_change(contrast_cmyc_high, "c-Myc (high peak)"),
  rna_change
) |>
  mutate(tx       = factor(tx, levels = tx_names),
         time     = factor(time, levels = c("w3", "post")),
         credible = lo > 0 | hi < 0)


# ============================================================
# Panels
# ============================================================

make_change_panel <- function(dat, title, legend = FALSE) {
  dat |>
    ggplot(aes(tx, est, color = time, shape = credible)) +
    geom_hline(yintercept = 0, linetype = 2, color = "grey40") +
    geom_pointrange(aes(ymin = lo, ymax = hi),
                    position = position_dodge(width = 0.5), size = 0.6, linewidth = 0.8) +
    scale_x_discrete(labels = tx_labels) +
    # drop = FALSE keeps the "Post" key in the protein panels, which have no post data
    scale_color_manual(values = c(w3 = "grey60", post = "steelblue"),
                       labels = c(w3 = "Week 3", post = "Post"), drop = FALSE, name = NULL) +
    scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1),
                       labels = c(`TRUE` = "95% interval excludes 0", `FALSE` = "95% interval includes 0"),
                       name = NULL) +
    labs(x = NULL, y = "Change from baseline (%)", title = title) +
    theme_classic() +
    theme(plot.title = element_text(face = "bold", size = 11),
          legend.position = if (legend) "bottom" else "none",
          legend.box = "vertical",
          legend.spacing.y = unit(0, "pt"))
}

p_ubf  <- make_change_panel(filter(change_dat, outcome == "UBF"), "UBF")
p_cmyc <- make_change_panel(filter(change_dat, outcome == "c-Myc (high peak)"), "c-Myc (high peak)")
p_rna  <- make_change_panel(filter(change_dat, outcome == "Total RNA"), "Total RNA", legend = TRUE)

# Lines: population-level predictions from m_corr (common UBF slope, time offset),
# drawn over the observed UBF range of each time point
p_scatter <- rna_ubf |>
  ggplot(aes(ystd, rna_mg, fill = time)) +
  geom_line(data = corr_lines, aes(ystd, rna_mg, group = time),
            color = "black", linewidth = 0.8, inherit.aes = FALSE) +
  geom_point(shape = 24, size = 2.5, color = "black", alpha = 0.9) +
  scale_fill_manual(values = c(pre = "white", w3 = "grey45"),
                    labels = c(pre = "Pre", w3 = "Week 3"), name = NULL) +
  labs(x = "UBF (log corrected area, SD units)",
       y = expression("Total RNA (ng" %.% "mg"^-1*")"),
       title = "Total RNA vs. UBF") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", size = 11),
        legend.position = "bottom")


# ============================================================
# Combine: three change panels on top, scatter centred below
# ============================================================

p_fig <- wrap_plots(
  list(p_ubf, p_cmyc, p_rna, p_scatter),
  design  = "AABB\nCCDD",
  heights = c(1, 1.1)
) +
  plot_annotation(tag_levels = "A")

p_fig

ggsave("figures/fig-protein-rna.png", p_fig, width = 16, height = 12, unit = "cm",
       dpi = 300)
saveRDS(p_fig, "figures/fig-protein-rna.RDS")