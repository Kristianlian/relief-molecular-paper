## Combined figure: change from baseline (UBF, c-Myc, total RNA) + total RNA vs. UBF

library(tidyverse)
library(patchwork)
library(reliefdata)

# --- Load saved model output ---
contrast_ubf         <- readRDS("data/data-gen/contrast_ubf.rds")          # Run A
contrast_cmyc_high   <- readRDS("data/data-gen/contrast_cmyc_high.rds")    # Run A
contrast_summary_rna <- readRDS("data/data-gen/rna_mg_contrast_summary_m4.rds")

# --- Groups ---
tx_names  <- c("int_old_low", "int_old_mod", "int_yng_low", "int_yng_mod", "con_old_con")
tx_labels <- c(int_old_low = "Old\nLow",   int_old_mod = "Old\nMod",
               int_yng_low = "Young\nLow", int_yng_mod = "Young\nMod",
               con_old_con = "Old\nControl")


# ============================================================
# Data for the scatter: UBF (Run A) paired with total RNA
# ============================================================

# UBF, Run A: same filters as make_prot_dat() in protein-runs.qmd
ubf <- reliefdata::relief_western |>
  mutate(participant = as.character(participant)) |>
  select(participant, leg, sampleid = sample, time, protein, area, corr.area) |>
  filter(!sampleid %in% c("Biot. Ladder", "DDH2O"),
         !participant %in% c("3084", "3090"),     # no measurement
         !is.na(sampleid), protein == "UBF") |>
  mutate(across(c(area, corr.area), as.numeric),
         norm_factor = area / corr.area) |>
  filter(norm_factor >= 0) |>                     # Run A: min_factor = 0 (drops invalid ratios)
  mutate(y    = log(corr.area),
         ystd = (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE),
         time = factor(time, levels = c("pre", "w3")))

# Check: should equal n for UBF run A in prot_runs
nrow(ubf)

# Total RNA per sample (mean of the spectrophotometer readings), ng per mg tissue
rna_samp <- reliefdata::relief_rnaconc |>
  mutate(participant = as.character(participant),
         total.rna   = conc * 4 * 25,
         weight      = as.numeric(weight)) |>
  filter(!is.na(participant), time %in% c("pre", "w3")) |>
  summarise(.by = c(participant, leg, time),
            rna = mean(total.rna, na.rm = TRUE), weight = first(weight)) |>
  mutate(rna_mg = rna / weight)

# Week 3: pair by participant and leg. Baseline: one UBF sample per participant,
# so pair with the participant's mean baseline RNA (works whichever leg it came from)
rna_pre <- rna_samp |> filter(time == "pre") |>
  summarise(.by = participant, rna_mg = mean(rna_mg))
rna_w3  <- rna_samp |> filter(time == "w3") |> select(participant, leg, rna_mg)

rna_ubf <- bind_rows(
  ubf |> filter(time == "pre") |> inner_join(rna_pre, by = "participant"),
  ubf |> filter(time == "w3")  |> inner_join(rna_w3,  by = c("participant", "leg"))
)

# Check: UBF rows vs. rows paired with RNA (expect few or none lost)
ubf |> count(time) |>
  left_join(rna_ubf |> count(time, name = "paired"), by = "time")


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

# Total RNA (model 4): baseline -> week 3 and baseline -> post
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
    theme_classic() +
    theme(plot.title = element_text(face = "bold", size = 11),
          legend.position = if (legend) "bottom" else "none",
          legend.box = "vertical",
          legend.spacing.y = unit(0, "pt"))
}

p_ubf  <- make_change_panel(filter(change_dat, outcome == "UBF"), "UBF")
p_cmyc <- make_change_panel(filter(change_dat, outcome == "c-Myc (high peak)"), "c-Myc (high peak)")
p_rna  <- make_change_panel(filter(change_dat, outcome == "Total RNA"), "Total RNA", legend = TRUE)

p_scatter <- rna_ubf |>
  ggplot(aes(ystd, rna_mg, fill = time)) +
  geom_smooth(aes(group = time), method = "lm", se = FALSE, color = "black", linewidth = 0.8) +
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
