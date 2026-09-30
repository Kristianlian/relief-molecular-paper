## Peak torque (HUMAC) — summary tables

library(tidyverse)
library(gt)

within_table_pct <- readRDS("data/data-gen/torque_within_pct.rds")
speed_table_pct  <- readRDS("data/data-gen/torque_speed_contrasts_pct.rds")

# Check the exact contrast strings if any header below doesn't get relabelled
# unique(speed_table_pct$contrast)

tx_labels <- c(
  int_old_low = "Old, Low",
  int_old_mod = "Old, Mod",
  int_yng_low = "Young, Low",
  int_yng_mod = "Young, Mod",
  con_old_con = "Old, Control"
)

# Definitions of each contrast (direction = first-named minus second-named).
# Names must match speed_table_pct$contrast exactly; unmatched ones keep their original label.
contrast_defs <- c(
  "Age (low volume)"           = "Age, low volume: Old \u2212 Young",
  "Age (mod volume)"           = "Age, mod volume: Old \u2212 Young",
  "Age (pooled across volume)" = "Age, pooled: Young \u2212 Old",
  "Volume (younger)"           = "Volume, young: Mod \u2212 Low",
  "Volume (older)"             = "Volume, old: Mod \u2212 Low",
  "Volume \u00d7 age interaction" = "Volume \u00d7 age: (Young Mod \u2212 Young Low) \u2212 (Old Mod \u2212 Old Low)",
  "Intervention vs control (low, older)"  = "Old low vs. control: Intervention \u2212 Control",
  "Intervention vs control (mod, older)"  = "Old mod vs. control: Intervention \u2212 Control",
  "Intervention vs control (pooled across volume, older)" = "Old pooled vs. control: Intervention \u2212 Control"
)

relabel <- function(d) {
  d |> mutate(contrast = coalesce(contrast_defs[as.character(contrast)],
                                  as.character(contrast)))
}

pos_note <- paste0(
  "Contrasts are first-named minus second-named group. ",
  "pos = posterior probability that the contrast is > 0, ",
  "so pos near 0 means strong evidence that the contrast is negative."
)



# --- Table 1: within-group % change, by group/speed/timepoint ---

within_group <- within_table_pct |>
  filter(timepoint %in% c("mid", "post")) |>
  mutate(
    group     = tx_labels[tx],
    timepoint = factor(timepoint, levels = c("mid", "post"), labels = c("Pre-Mid", "Pre-Post"))
  ) |>
  mutate(group = factor(group, levels = tx_labels)) |>
  arrange(group, speed_label, timepoint) |>
  select(group, speed_label, timepoint, mean, lower95, upper95, pos, credible)

table_torque_within <- within_group |>
  gt(groupname_col = "group") |>
  fmt_number(columns = c(mean, lower95, upper95), decimals = 1) |>
  fmt_number(columns = pos, decimals = 2) |>
  cols_merge(columns = c(mean, lower95, upper95),
             pattern = "{1}% [{2}%, {3}%]") |>
  cols_label(
    speed_label = "Speed",
    timepoint   = "Timepoint",
    mean        = "\u0394 (%, 95% ETI)",
    pos         = "pos",
    credible    = "Credible"
  ) |>
  tab_header(title = "Peak torque: within-group % change, by speed and timepoint") |>
  tab_source_note("pos = posterior probability that the change is > 0.")

table_torque_within

# --- Table 2: between-group contrasts, by contrast/speed/timepoint ---

between_group <- speed_table_pct |>
  filter(timepoint %in% c("mid", "post")) |>
  mutate(timepoint = factor(timepoint, levels = c("mid", "post"), labels = c("Pre-Mid", "Pre-Post"))) |>
  arrange(contrast, speed_label, timepoint) |>
  select(contrast, speed_label, timepoint, mean, lower95, upper95, pos, credible) |>
  relabel()

table_torque_between <- between_group |>
  gt(groupname_col = "contrast") |>
  fmt_number(columns = c(mean, lower95, upper95), decimals = 1) |>
  fmt_number(columns = pos, decimals = 2) |>
  cols_merge(columns = c(mean, lower95, upper95),
             pattern = "{1}% [{2}%, {3}%]") |>
  cols_label(
    speed_label = "Speed",
    timepoint   = "Timepoint",
    mean        = "\u0394 (%, 95% ETI)",
    pos         = "pos",
    credible    = "Credible"
  ) |>
  tab_header(title = "Peak torque: between-group contrasts, by speed and timepoint") |>
  tab_source_note(pos_note)

table_torque_between

saveRDS(table_torque_within,  "figures/tables/tab-torque-within.RDS")
saveRDS(table_torque_between, "figures/tables/tab-torque-between.RDS")
gtsave(table_torque_within,  "figures/tables/tab-torque-within.html")
gtsave(table_torque_between, "figures/tables/tab-torque-between.html")

# --- Table 3: between-group contrasts, credible only ---

between_group_credible <- between_group |>
  filter(credible == "Yes") |>
  select(-credible)

table_torque_between_credible <- between_group_credible |>
  gt(groupname_col = "contrast") |>
  fmt_number(columns = c(mean, lower95, upper95), decimals = 1) |>
  fmt_number(columns = pos, decimals = 2) |>
  cols_merge(columns = c(mean, lower95, upper95),
             pattern = "{1}% [{2}%, {3}%]") |>
  cols_label(
    speed_label = "Speed",
    timepoint   = "Timepoint",
    mean        = "\u0394 (%, 95% ETI)",
    pos         = "pos"
  ) |>
  tab_header(title = "Peak torque: credible between-group contrasts only") |>
  tab_source_note(pos_note)

table_torque_between_credible

saveRDS(table_torque_between_credible, "figures/tables/tab-torque-between-credible.RDS")
gtsave(table_torque_between_credible, "figures/tables/tab-torque-between-credible.html")
