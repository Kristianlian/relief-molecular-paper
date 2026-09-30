# --- Glucose Tolerance Table ---

library(tidyverse)
library(gt)

comp_df <- readRDS("data/data-gen/comp_df_ogtt.rds")

lab <- c(
  yng        = "Within group: Young intervention (post - pre)",
  old        = "Within group: Old intervention (post - pre)",
  con        = "Within group: Old control (post - pre)",
  old_vs_con = "Between groups: Old intervention vs. control",
  yng_vs_old = "Between groups: Young vs. old (training response)"
)
dir_neg <- c(yng = "Reduction", old = "Reduction", con = "Reduction",
             old_vs_con = "Control > Intervention", yng_vs_old = "Old > Young")
dir_pos <- c(yng = "Increase", old = "Increase", con = "Increase",
             old_vs_con = "Intervention > Control", yng_vs_old = "Young > Old")

# --- Summaries per quantity and time point ---
summ <- comp_df |>
  select(drawid, sampletime, all_of(names(lab))) |>
  pivot_longer(-c(drawid, sampletime), names_to = "contrast", values_to = "value") |>
  summarise(.by = c(contrast, sampletime),
            m   = mean(value, na.rm = TRUE),
            l95 = quantile(value, 0.025, na.rm = TRUE),
            u95 = quantile(value, 0.975, na.rm = TRUE),
            pos = if_else(m > 0, mean(value > 0, na.rm = TRUE),
                          mean(value < 0, na.rm = TRUE)))

# --- Contiguous credible windows, with the peak inside each window ---
windows_raw <- summ |>
  mutate(credible  = l95 > 0 | u95 < 0,
         direction = if_else(m > 0, unname(dir_pos[contrast]), unname(dir_neg[contrast]))) |>
  filter(credible) |>
  arrange(contrast, direction, sampletime) |>
  mutate(.by = c(contrast, direction),
         run_id = cumsum(sampletime - lag(sampletime, default = first(sampletime)) > 2)) |>
  summarise(.by = c(contrast, direction, run_id),
            start_time = min(sampletime),
            end_time   = max(sampletime),
            peak_time  = sampletime[which.max(abs(m))],
            estimate   = sprintf("%.2f [%.2f, %.2f]",
                                 m[which.max(abs(m))], l95[which.max(abs(m))], u95[which.max(abs(m))]),
            pos        = pos[which.max(abs(m))]) |>
  select(-run_id)

# --- Groups with no credible window: report the overall largest change ---
none_raw <- summ |>
  slice_max(abs(m), n = 1, by = contrast) |>
  filter(!contrast %in% windows_raw$contrast) |>
  mutate(direction  = paste0(if_else(m > 0, unname(dir_pos[contrast]), unname(dir_neg[contrast])),
                             " (not credible)"),
         start_time = NA_real_,
         end_time   = NA_real_,
         peak_time  = sampletime,
         estimate   = sprintf("%.2f [%.2f, %.2f]", m, l95, u95)) |>
  select(contrast, direction, start_time, end_time, peak_time, estimate, pos)

windows_tbl <- bind_rows(windows_raw, none_raw) |>
  mutate(label = factor(lab[contrast], levels = lab)) |>
  arrange(label, start_time) |>
  select(label, direction, start_time, end_time, peak_time, estimate, pos)

# --- Overall largest change per quantity ---
max_change <- summ |>
  slice_max(abs(m), n = 1, by = contrast) |>
  mutate(txt = sprintf("%s: largest change at %d min (%.2f mmol L\u207b\u00b9, 95%% CI [%.2f, %.2f], pos = %.1f%%)",
                       lab[contrast], sampletime, m, l95, u95, 100 * pos),
         contrast = factor(contrast, levels = names(lab))) |>
  arrange(contrast)

# --- Table ---
table_ogtt <- windows_tbl |>
  gt(groupname_col = "label") |>
  fmt_number(columns = c(start_time, end_time, peak_time), decimals = 0) |>
  fmt_percent(columns = pos, decimals = 1) |>
  sub_missing(columns = everything(), missing_text = "\u2013") |>
  cols_label(direction  = "Direction",
             start_time = "Window start (min)",
             end_time   = "Window end (min)",
             peak_time  = "Peak (min)",
             estimate   = "Peak estimate, mmol L\u207b\u00b9 [95% CI]",
             pos        = "pos") |>
  tab_header(title = "OGTT: credible windows of within- and between-group differences") |>
  tab_source_note("Peak = largest mean change within each credible window. For groups without a credible window, the overall largest change is shown. pos = posterior probability in the direction of the estimate.")

table_ogtt

saveRDS(table_ogtt, "figures/tables/tab-ogtt.RDS")
gtsave(table_ogtt, "figures/tables/tab-ogtt.html")