## 1RM leg-press — summary table (within-group + between-group contrasts)

library(tidyverse)
library(gt)

pred_1rm     <- readRDS("data/data-gen/pred_1rm.rds")
contrast_1rm <- readRDS("data/data-gen/contrast_1rm.rds")

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

# --- Section 1: within-group change, both timepoints ---

baseline_vals <- pred_1rm |>
  filter(time == "pre") |>
  summarise(.by = tx, baseline = mean(m))

within_group <- contrast_1rm |>
  filter(hypothesis %in% paste0(rep(names(tx_labels), each = 2), "_delta_", c("mid", "post"))) |>
  mutate(
    tx        = str_remove(hypothesis, "_delta_(mid|post)$"),
    timepoint = str_extract(hypothesis, "(mid|post)$"),
    timepoint = factor(timepoint, levels = c("mid", "post"), labels = c("Pre-Mid", "Pre-Post"))
  ) |>
  left_join(baseline_vals, by = "tx") |>
  mutate(
    section     = "Within-group change",
    row_label   = paste(tx_labels[tx], timepoint, sep = " | "),
    abs_change  = (mean - 1) * baseline,
    abs_lower95 = (lower95 - 1) * baseline,
    abs_upper95 = (upper95 - 1) * baseline
  ) |>
  mutate(tx = factor(tx, levels = names(tx_labels))) |>
  arrange(tx, timepoint) |>
  mutate(row_label = factor(row_label, levels = unique(row_label))) |>
  select(section, row_label, baseline, abs_change, abs_lower95, abs_upper95,
         pct_change, pct_lower, pct_upper, pos)

# --- Section 2: between-group contrasts, both timepoints ---

between_group <- contrast_1rm |>
  filter(hypothesis %in% paste0(rep(key_contrasts, each = 2), "_", c("mid", "post"))) |>
  mutate(
    base      = str_remove(hypothesis, "_(mid|post)$"),
    timepoint = str_extract(hypothesis, "(mid|post)$"),
    timepoint = factor(timepoint, levels = c("mid", "post"), labels = c("Pre-Mid", "Pre-Post")),
    section   = "Between-group contrasts",
    row_label = paste(contrast_labels[base], timepoint, sep = " | "),
    baseline    = NA_real_,
    abs_change  = NA_real_,
    abs_lower95 = NA_real_,
    abs_upper95 = NA_real_
  ) |>
  mutate(base = factor(base, levels = key_contrasts)) |>
  arrange(base, timepoint) |>
  mutate(row_label = factor(row_label, levels = unique(row_label))) |>
  select(section, row_label, baseline, abs_change, abs_lower95, abs_upper95,
         pct_change, pct_lower, pct_upper, pos)

# --- Combine ---

table_data <- bind_rows(within_group, between_group) |>
  mutate(section = factor(section, levels = c("Within-group change", "Between-group contrasts")))

table_1rm <- table_data |>
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
    baseline   = "Baseline (kg)",
    abs_change = "\u0394 (kg, 95% ETI)",
    pct_change = "\u0394 (%, 95% ETI)",
    pos        = "pos"
  ) |>
  tab_header(title = "1RM Leg-Press: change by group and timepoint, and between-group contrasts")

table_1rm


saveRDS(table_1rm, "figures/tables/tab-leg-press.RDS")
gtsave(table_1rm, "figures/tables/tab-leg-press.html")

