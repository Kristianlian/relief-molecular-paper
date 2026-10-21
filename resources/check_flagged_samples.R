# Go through every sample with a QC note. 
# Needs flagged_samples.csv (sample, type, purity_note, unsure, plate_well_note).
#   type: curve_lowpurity | curve_only | small | small_curve | NS

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

notes <- read_csv("./resources/flagged_samples.csv", show_col_types = FALSE) |>
  mutate(sample = as.character(sample)) |>
  distinct(sample, .keep_all = TRUE)            # 169 is listed twice; kept once

# ---- Raw wells, with group labels (same joins as your total RNA qmd) ------------------
wells <- reliefdata::relief_rnaconc |>
  mutate(participant = as.character(participant), sample = as.character(sample)) |>
  filter(!is.na(participant)) |>
  mutate(total.rna = conc * 4 * 25, weight = as.numeric(weight))

meta <- reliefdata::relief_volume |> mutate(participant = as.character(participant)) |> select(-arm) |>
  full_join(reliefdata::relief_participants |>
              mutate(participant = as.character(participant),
                     age_group = if_else(age < 40, "yng", "old")) |>
              select(participant, allocation, age_group, sex),
            by = "participant") |>
  mutate(condition = if_else(allocation == "con", "con", condition),
         tx = paste0(allocation, "_", age_group, "_", condition))

wells <- wells |> left_join(meta, by = c("participant", "leg"))

# ---- 1. Every flagged sample: all wells side by side ---------------------------------
flag_wells <- wells |>
  semi_join(notes, by = "sample") |>
  group_by(sample) |>
  mutate(med_conc = median(conc, na.rm = TRUE),
         dev_from_median = conc / med_conc,                 # 1 = same as the median well
         worst_well = abs(log(conc / med_conc)) == max(abs(log(conc / med_conc)), na.rm = TRUE),
         cv = sd(conc, na.rm = TRUE) / mean(conc, na.rm = TRUE)) |>
  ungroup() |>
  left_join(notes, by = "sample") |>
  arrange(as.numeric(sample), plate, well) |>
  select(sample, type, participant, leg, time, tx, weight, plate, well, conc, purity, abs260,
         abs280, abs230, dev_from_median, worst_well, cv, purity_note, unsure, plate_well_note)

print(flag_wells, n = Inf, width = Inf)
write_csv(flag_wells, "flagged_samples_wells.csv")        # open this next to your lab notes

# Sample-level summary: does the rest of the sample hold up if the flagged well is dropped?
flag_summary <- flag_wells |>
  group_by(sample, type, participant, leg, time, tx, weight, unsure) |>
  summarise(n_wells = n(),
            mean_all = mean(conc), mean_drop_worst = mean(conc[!worst_well]),
            change_pct = 100 * (mean_drop_worst / mean_all - 1),
            min_purity = min(purity, na.rm = TRUE), cv = first(cv), .groups = "drop")
print(flag_summary, n = Inf, width = Inf)

# ---- 2. Are flags clustered by timepoint, group or run? ------------------------------
all_samples <- wells |> distinct(sample, participant, leg, time, tx) |>
  left_join(notes |> select(sample, type), by = "sample") |>
  mutate(flagged = !is.na(type), sample_n = as.numeric(sample))

all_samples |> count(time, flagged) |> pivot_wider(names_from = flagged, values_from = n)
all_samples |> count(tx, flagged) |> pivot_wider(names_from = flagged, values_from = n)
all_samples |> mutate(block = cut(sample_n, c(0, 100, 150, 200, 250, 300, 350, 500))) |>
  count(block, flagged) |> pivot_wider(names_from = flagged, values_from = n)

# Purity and concentration by timepoint and group, ALL samples (a batch effect would show here)
wells |> group_by(time) |>
  summarise(med_purity = median(purity, na.rm = TRUE), med_conc = median(conc, na.rm = TRUE),
            pct_purity_lt18 = 100 * mean(purity < 1.8, na.rm = TRUE), n = n())
wells |> group_by(tx, time) |>
  summarise(med_purity = median(purity, na.rm = TRUE), pct_purity_lt18 = 100 * mean(purity < 1.8, na.rm = TRUE), .groups = "drop") |>
  print(n = Inf)

ggplot(wells, aes(as.numeric(sample), purity, colour = time)) + geom_point(alpha = .5) +
  geom_hline(yintercept = 1.8, linetype = 2) + labs(x = "Sample number", y = "Purity (A260/280)")

# ---- 3. Sensitivity: does the result move with the flagged data removed? --------------
make_chg <- function(w) {
  w |> group_by(participant, leg, time, tx) |>
    summarise(mean_rna = mean(total.rna, na.rm = TRUE), weight = first(weight), .groups = "drop") |>
    filter(!is.na(mean_rna), !is.na(weight)) |>
    mutate(rna_mg = mean_rna / weight, time = factor(time, c("pre", "w3", "post"))) |>
    select(participant, leg, time, tx, rna_mg) |>
    pivot_wider(names_from = time, values_from = rna_mg) |>
    mutate(pct_w3 = 100 * (w3 / pre - 1), pct_post = 100 * (post / pre - 1))
}
summ <- function(chg, label)
  chg |> group_by(tx) |>
    summarise(variant = label,
              n_w3 = sum(!is.na(pct_w3)),   mean_w3 = mean(pct_w3, na.rm = TRUE),   med_w3 = median(pct_w3, na.rm = TRUE),
              n_post = sum(!is.na(pct_post)), mean_post = mean(pct_post, na.rm = TRUE), med_post = median(pct_post, na.rm = TRUE),
              .groups = "drop")

worst_ids <- flag_wells |> filter(worst_well, type %in% c("NS", "curve_lowpurity", "curve_only", "small_curve")) |>
  select(sample, plate, well)
small_ids <- notes |> filter(grepl("small", type)) |> pull(sample)
ns_ids    <- flag_wells |> filter(type == "NS", worst_well) |> select(sample, plate, well)

variants <- bind_rows(
  summ(make_chg(wells), "as is"),
  summ(make_chg(wells |> anti_join(ns_ids, by = c("sample", "plate", "well"))), "drop NS wells"),
  summ(make_chg(wells |> anti_join(worst_ids, by = c("sample", "plate", "well"))), "drop flagged worst wells"),
  summ(make_chg(wells |> filter(!sample %in% small_ids)), "drop all 'small' samples"),
  summ(make_chg(wells |> filter(!sample %in% notes$sample)), "drop all flagged samples")
)
print(variants |> arrange(tx, variant), n = Inf, width = Inf)

# The sample that started this: 3084 R, week 3
wells |> filter(participant == "3084", leg == "R") |>
  select(sample, time, plate, well, conc, purity, weight) |> arrange(time, plate, well)

# ----------------- Well position check ----------------------------
# Follow-up to check_flagged_samples.R. 
# Uses `wells` and `notes` from that script (run it first).
# 1) Is the odd well almost always the 4th well of the block, in ALL samples (not only flagged ones)?
# 2) Apply one well-level rule to every sample and rebuild the group changes.
# 3) Look for other failed extractions (very low RNA per mg).


# ---- 1. Position of each well within its sample (A02, A03, B02, B03 -> 1..4) -----------
pos <- wells |>
  arrange(plate, well) |>
  group_by(sample) |>
  mutate(pos = row_number(),
         med_others = sapply(seq_along(conc), \(i) median(conc[-i], na.rm = TRUE)),
         dev = conc / med_others) |>
  ungroup() |>
  mutate(plate_n = as.numeric(gsub("\\D", "", plate)))

# How often is a well >25% off the other three, by position? (if it is mostly pos 4 -> positional artifact)
pos |> mutate(off = abs(log(dev)) > log(1.25) | purity < 1.5) |>
  group_by(pos) |> summarise(n = n(), pct_off = 100 * mean(off, na.rm = TRUE))

# Same, by plate block (is it the 81-101 run only?)
pos |> filter(pos == 4) |>
  mutate(block = cut(plate_n, c(0, 40, 60, 80, 100, 120)), off = abs(log(dev)) > log(1.25) | purity < 1.5) |>
  group_by(block) |> summarise(n = n(), pct_off = 100 * mean(off, na.rm = TRUE))

# ---- 2. One rule for every sample -----------------------------------------------------
# Drop a well if it is >25% from the median of the other three AND purity < 1.8 (or purity < 1.5).
clean <- pos |>
  mutate(drop = (abs(log(dev)) > log(1.25) & purity < 1.8) | purity < 1.5) |>
  filter(!drop | is.na(drop))
cat("wells dropped:", nrow(pos) - nrow(clean), "of", nrow(pos), "\n")

# Sample-level failure: very low RNA per mg (conc / weight) compared with the cohort
persample <- clean |> group_by(sample, participant, leg, time, tx, weight) |>
  summarise(conc = median(conc), .groups = "drop") |>      # median of remaining wells is robust
  mutate(conc_mg = conc / weight)
persample |> arrange(conc_mg) |> select(sample, participant, leg, time, tx, weight, conc, conc_mg) |> head(20)
median(persample$conc_mg, na.rm = TRUE)
# How does 3084 R compare across its own three timepoints?
persample |> filter(participant == "3084", leg == "R")

# ---- 3. Rebuild the group changes with the clean wells, sample 349 in and out ---------
chg_from <- function(ps) {
  ps |> mutate(time = factor(time, c("pre", "w3", "post"))) |>
    select(participant, leg, time, tx, conc_mg) |>
    pivot_wider(names_from = time, values_from = conc_mg) |>
    mutate(pct_w3 = 100 * (w3 / pre - 1), pct_post = 100 * (post / pre - 1))
}
summ2 <- function(chg, label)
  chg |> group_by(tx) |>
  summarise(variant = label,
            n_w3 = sum(!is.na(pct_w3)), mean_w3 = mean(pct_w3, na.rm = TRUE), med_w3 = median(pct_w3, na.rm = TRUE),
            n_post = sum(!is.na(pct_post)), mean_post = mean(pct_post, na.rm = TRUE), med_post = median(pct_post, na.rm = TRUE),
            .groups = "drop")
bind_rows(
  summ2(chg_from(persample), "clean wells, 349 in"),
  summ2(chg_from(persample |> filter(sample != "349")), "clean wells, 349 out")
) |> arrange(tx, variant) |> print(n = Inf, width = Inf)

# If 349's weight is a typo (12 vs 1.2?), see what the w3 change would be:
persample |> filter(sample == "349") |> mutate(conc_mg_if_1.2mg = conc / 1.2)
