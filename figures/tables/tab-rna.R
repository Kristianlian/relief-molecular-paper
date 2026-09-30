## Total RNA — summary table (within-group + between-group contrasts)

library(tidyverse)
library(gt)

pred_rna              <- readRDS("data/data-gen/pred_rna.rds")
contrast_summary_rep  <- readRDS("data/data-gen/rna_mg_contrast_summary_m4.rds")
contrast_rna          <- readRDS("data/data-gen/contrast_rna.rds")

tx_labels <- c(
  int_old_low = "Old, Low", int_old_mod = "Old, Mod",
  int_yng_low = "Young, Low", int_yng_mod = "Young, Mod",
  con_old_con = "Old, Control"
)

key_contrasts <- c("old_vs_con_delta", "yng_vs_old_delta",
                   "yng_mod_vs_low", "old_mod_vs_low", "yng_vs_old_vol")

contrast_labels <- c(
  old_vs_con_delta = "Old: Intervention vs. Control",
  yng_vs_old_delta = "Young vs. Old (intervention)",
  yng_mod_vs_low   = "Young: Mod vs. Low",
  old_mod_vs_low   = "Old: Mod vs. Low",
  yng_vs_old_vol   = "Volume effect: Young vs. Old"
)

# --- Section 1: within-group change, from the full contrast_summary_rep ---

baseline_vals <- pred_rna |> filter(time == "pre") |> summarise(.by = tx, baseline = mean(m))

within_group <- contrast_summary_rep |>
  filter(contrast %in% paste0(rep(names(tx_labels), each = 2), "_", c("w3", "post"))) |>
  mutate(
    tx        = str_remove(contrast, "_(w3|post)$"),
    timepoint = str_extract(contrast, "(w3|post)$"),
    timepoint = factor(timepoint, levels = c("w3", "post"), labels = c("Pre-W3", "Pre-Post"))
  ) |>
  left_join(baseline_vals, by = "tx") |>
  mutate(
    section     = "Within-group change",
    row_label   = paste(tx_labels[tx], timepoint, sep = " | "),
    abs_change  = (fc_est   - 1) * baseline,
    abs_lower95 = (fc_lower - 1) * baseline,
    abs_upper95 = (fc_upper - 1) * baseline,
    pct_change  = (fc_est   - 1) * 100,
    pct_lower   = (fc_lower - 1) * 100,
    pct_upper   = (fc_upper - 1) * 100
  ) |>
  mutate(tx = factor(tx, levels = names(tx_labels))) |>
  arrange(tx, timepoint) |>
  mutate(row_label = factor(row_label, levels = unique(row_label))) |>
  select(section, row_label, baseline, abs_change, abs_lower95, abs_upper95,
         pct_change, pct_lower, pct_upper, pos)

# --- Section 2: between-group contrasts, from contrast_rna (already fold-change-scaled, no baseline) ---

between_group <- contrast_rna |>
  filter(hypothesis %in% key_contrasts) |>
  mutate(
    timepoint = factor(delta, levels = c("pre to w3", "pre to post"), labels = c("Pre-W3", "Pre-Post")),
    section   = "Between-group contrasts",
    row_label = paste(contrast_labels[hypothesis], timepoint, sep = " | "),
    pct_change = (mean    - 1) * 100,
    pct_lower  = (lower95 - 1) * 100,
    pct_upper  = (upper95 - 1) * 100,
    baseline    = NA_real_, abs_change = NA_real_, abs_lower95 = NA_real_, abs_upper95 = NA_real_
  ) |>
  mutate(hypothesis = factor(hypothesis, levels = key_contrasts)) |>
  arrange(hypothesis, timepoint) |>
  mutate(row_label = factor(row_label, levels = unique(row_label))) |>
  select(section, row_label, baseline, abs_change, abs_lower95, abs_upper95,
         pct_change, pct_lower, pct_upper, pos)

# --- Combine ---

table_data <- bind_rows(within_group, between_group) |>
  mutate(section = factor(section, levels = c("Within-group change", "Between-group contrasts")))

table_rna <- table_data |>
  gt(groupname_col = "section") |>
  fmt_number(columns = c(baseline, abs_change, abs_lower95, abs_upper95), decimals = 0) |>
  fmt_number(columns = c(pct_change, pct_lower, pct_upper), decimals = 1) |>
  fmt_number(columns = pos, decimals = 2) |>
  sub_missing(columns = c(baseline, abs_change, abs_lower95, abs_upper95), missing_text = "\u2014") |>
  cols_merge(columns = c(abs_change, abs_lower95, abs_upper95), pattern = "{1} [{2}, {3}]") |>
  cols_merge(columns = c(pct_change, pct_lower, pct_upper), pattern = "{1}% [{2}%, {3}%]") |>
  cols_label(row_label = "Group / Contrast", baseline = "Baseline (ng)",
             abs_change = "\u0394 (ng, 95% ETI)", pct_change = "\u0394 (%, 95% ETI)", pos = "pos") |>
  tab_header(title = "Total RNA: change by group and timepoint, and between-group contrasts")

table_rna


saveRDS(table_rna, "figures/tables/tab-total-rna.RDS")
gtsave(table_rna, "figures/tables/tab-total-rna.html")


