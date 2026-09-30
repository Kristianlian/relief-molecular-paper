## Muscle volume — summary table (within-group + between-group contrasts)

library(tidyverse)
library(gt)

pred_mv     <- readRDS("data/data-gen/pred_mv.rds")
contrast_mv <- readRDS("data/data-gen/contrast_mv.rds")

tx_labels <- c(
  int_old_low = "Old, Low",
  int_old_mod = "Old, Mod",
  int_yng_low = "Young, Low",
  int_yng_mod = "Young, Mod",
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

# --- Section 1: within-group change ---

baseline_vals <- pred_mv |>
  filter(time == "pre") |>
  summarise(.by = tx, baseline = mean(m))

within_group <- contrast_mv |>
  filter(hypothesis %in% paste0(names(tx_labels), "_delta")) |>
  mutate(tx = str_remove(hypothesis, "_delta$")) |>
  left_join(baseline_vals, by = "tx") |>
  mutate(
    section     = "Within-group change",
    row_label   = tx_labels[tx],
    baseline    = baseline,
    abs_change  = (mean - 1) * baseline,
    abs_lower95 = (lower95 - 1) * baseline,
    abs_upper95 = (upper95 - 1) * baseline
  ) |>
  mutate(row_label = factor(row_label, levels = tx_labels)) |>
  arrange(row_label) |>
  select(section, row_label, baseline, abs_change, abs_lower95, abs_upper95,
         pct_change, pct_lower, pct_upper, pos)

# --- Section 2: between-group contrasts (no baseline/absolute — different reference groups) ---

between_group <- contrast_mv |>
  filter(hypothesis %in% key_contrasts) |>
  mutate(
    section     = "Between-group contrasts",
    row_label   = contrast_labels[hypothesis],
    baseline    = NA_real_,
    abs_change  = NA_real_,
    abs_lower95 = NA_real_,
    abs_upper95 = NA_real_
  ) |>
  mutate(row_label = factor(row_label, levels = contrast_labels[key_contrasts])) |>
  arrange(row_label) |>
  select(section, row_label, baseline, abs_change, abs_lower95, abs_upper95,
         pct_change, pct_lower, pct_upper, pos)

# --- Combine ---

table_data <- bind_rows(within_group, between_group) |>
  mutate(section = factor(section, levels = c("Within-group change", "Between-group contrasts")))

table_mv <- table_data |>
  gt(groupname_col = "section") |>
  fmt_number(columns = c(baseline, abs_change, abs_lower95, abs_upper95), decimals = 1) |>
  fmt_number(columns = c(pct_change, pct_lower, pct_upper), decimals = 1) |>
  fmt_number(columns = pos, decimals = 2) |>
  sub_missing(columns = c(baseline, abs_change, abs_lower95, abs_upper95), missing_text = "\u2014") |>
  cols_merge(columns = c(abs_change, abs_lower95, abs_upper95),
             pattern = "{1} [{2}, {3}]") |>
  cols_merge(columns = c(pct_change, pct_lower, pct_upper),
             pattern = "{1}% [{2}%, {3}%]") |>
  cols_label(
    row_label  = "Group / Contrast",
    baseline   = "Baseline (cm\u00b3)",
    abs_change = "\u0394 (cm\u00b3, 95% ETI)",
    pct_change = "\u0394 (%, 95% ETI)",
    pos        = "pos"
  ) |>
  tab_header(title = "Muscle volume: pre-to-post change by group, and between-group contrasts")

table_mv


saveRDS(table_mv, "figures/tables/tab-muscle-volume.RDS")
gtsave(table_mv, "figures/tables/tab-muscle-volume.html")