# Total RNA: data import, QC and model data
# Sourced by total-rna.qmd (m4, m5) and total-rna-supplement.qmd (m0-m3),
# so that QC and exclusions are defined in one place.

library(tidyverse)
library(reliefdata)

# ---- Exclusions ----------------------------------------------------------------
# Excluded: completed the intervention with too few RT sessions (3017, 3092).
# Dropouts ("not completed") are kept; their baseline data contribute to the
# baseline comparisons.
excluded <- reliefdata::relief_participants |>
  filter(status == "incomplete") |>
  pull(participant) |>
  as.character()

# ---- Well-level concentration data with QC --------------------------------------
conc.dat <- reliefdata::relief_rnaconc |> 
  mutate(participant = as.character(participant)) |> 
  mutate(participant = ifelse(is.na(participant), "blank", participant),
         leg = ifelse(is.na(leg), "blank", leg),
         time = ifelse(is.na(time), "blank", time),
         weight = ifelse(is.na(weight), "0", weight)) |> 
  mutate(total.rna = conc * 4 * 25) |> 
  dplyr::select(participant, leg, time, weight, well, plate, sample, conc, purity, total.rna) |> 
  filter(participant != "blank") |> 
  # Well-level QC: drop a well that deviates >25% from the median of the
  # sample's other wells with purity < 1.8, or any well with purity < 1.5.
  # (The deviating well was almost always the 4th of the block, mainly on
  # plates 81-100; see check_flagged_samples.R.)
  group_by(sample) |> 
  mutate(med_others = sapply(seq_along(conc), \(i) median(conc[-i], na.rm = TRUE)),
         dev = conc / med_others,
         drop_well = (abs(log(dev)) > log(1.25) & purity < 1.8) | purity < 1.5) |> 
  ungroup() |> 
  filter(!drop_well | is.na(drop_well)) |> 
  # Sample 349 (3084 R w3): failed extraction, >6-fold below any other sample
  filter(sample != "349") |> 
  # Too few RT sessions (status "incomplete")
  filter(!participant %in% excluded) |> 
  dplyr::select(-conc, -purity, -med_others, -dev, -drop_well) |> 
  mutate(weight = as.numeric(weight))

# ---- Metadata --------------------------------------------------------------------
condition <- reliefdata::relief_volume |> 
  mutate(participant = as.character(participant))

participants <- reliefdata::relief_participants |> 
  mutate(age_group = if_else(age < 40, "yng", "old")) |> 
  dplyr::select(participant, age, sex, height, bodyweight = weight, allocation, age_group)

# Group factors used by all models
tx_levels <- c("int_old_low", "int_old_mod", "int_yng_low", "int_yng_mod", "con_old_con")

add_groups <- function(d) {
  d |> 
    mutate(
      condition = if_else(allocation == "con", "con", condition),
      tx = factor(paste0(allocation, "_", age_group, "_", condition), levels = tx_levels),
      tx_sex = factor(paste0(tx, "_", sex),
                      levels = paste0(rep(tx_levels, each = 2), c("_female", "_male"))),
      time = factor(time, levels = c("pre", "w3", "post")),
      sex_centered = if_else(sex == "female", -0.5, 0.5)
    )
}

# ---- Replicate-level data (m4, m5) -----------------------------------------------
# One row per well; replicate structure handled by (1 | participant:sample)
rna_rep.dat <- conc.dat |> 
  full_join(condition) |> 
  select(-arm) |> 
  full_join(participants) |> 
  mutate(
    y     = log(total.rna),
    ystd  = (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE),
    mg    = log(weight),
    mgstd = (mg - mean(mg, na.rm = TRUE)) / sd(mg, na.rm = TRUE)
  ) |> 
  add_groups() |> 
  filter(!is.na(y), !is.na(mg), !is.na(time))

# Saved for back-transformation
mean_rna_rep <- mean(rna_rep.dat$y, na.rm = TRUE)
sd_rna_rep   <- sd(rna_rep.dat$y, na.rm = TRUE)
mean_mg_rep  <- mean(rna_rep.dat$mg, na.rm = TRUE)
sd_mg_rep    <- sd(rna_rep.dat$mg, na.rm = TRUE)

# ---- Sample-level data (m0-m3, supplement) ----------------------------------------
# Technical replicates averaged per biopsy
rna.dat <- conc.dat |> 
  full_join(condition) |> 
  select(-arm) |> 
  full_join(participants) |> 
  group_by(participant, leg, time, age_group, allocation, condition, sex) |> 
  summarise(
    mean_rna = mean(total.rna, na.rm = TRUE), # average technical replicates
    weight = first(weight),                   # one weight measurement per sample
    .groups = "drop"
  ) |> 
  mutate(
    y     = log(mean_rna),
    ystd  = (y - mean(y, na.rm = TRUE)) / sd(y, na.rm = TRUE),
    mg    = log(weight),
    mgstd = (mg - mean(mg, na.rm = TRUE)) / sd(mg, na.rm = TRUE)
  ) |> 
  add_groups() |> 
  filter(!is.na(y), !is.na(mg), !is.na(time))

mean_rna <- mean(rna.dat$y, na.rm = TRUE)
sd_rna   <- sd(rna.dat$y, na.rm = TRUE)
mean_mg  <- mean(rna.dat$mg, na.rm = TRUE)
sd_mg    <- sd(rna.dat$mg, na.rm = TRUE)