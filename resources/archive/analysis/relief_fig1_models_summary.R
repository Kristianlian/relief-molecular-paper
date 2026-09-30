# =============================================================================
# RELIEF Study — Model Summary Script
# Fits all LMMs and extracts ANOVA tables + absolute/% change summaries
# =============================================================================

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

# =============================================================================
# LOAD DATA
# =============================================================================

iso_isom       <- readRDS("./data/data-gen/iso_isom.rds")
isom_int       <- readRDS("./data/data-gen/iso_isom_int.RDS")
isom_con       <- readRDS("./data/data-gen/iso_isom_con.rds")
iso_isok       <- readRDS("./data/data-gen/iso_isok.rds")
pt_60_int      <- readRDS("./data/data-gen/pt_60.rds")
pt_60_con      <- readRDS("./data/data-gen/pt_60_con.rds")
pt_120_int     <- readRDS("./data/data-gen/pt_120.rds")
pt_120_con     <- readRDS("./data/data-gen/pt_120_con.rds")
pt_240_int     <- readRDS("./data/data-gen/pt_240.rds")
pt_240_con     <- readRDS("./data/data-gen/pt_240_con.rds")
keiser_dat     <- readRDS("./data/data-gen/keiser_dat.rds")
keiser_int     <- readRDS("./data/data-gen/keiser_int.rds")
keiser_con     <- readRDS("./data/data-gen/keiser_con.rds")
bc_dat         <- readRDS("./data/data-gen/bc_dat.rds")
bc_int         <- readRDS("./data/data-gen/bc_int.rds")
bc_con         <- readRDS("./data/data-gen/bc_con.rds")
muscle_volume  <- readRDS("./data/data-gen/muscle_volume.RDS")
muscle_thickness <- readRDS("./data/data-gen/muscle_thickness.rds")
lean_dat       <- readRDS("./data/data-gen/lean_dat.rds")

# =============================================================================
# HELPERS
# =============================================================================

# Standardise factor levels across datasets
prep_factors <- function(df, time_levels = NULL) {
  if ("time" %in% names(df) && !is.factor(df$time)) {
    df$time <- factor(df$time, levels = time_levels %||% unique(df$time))
  }
  if ("age_grp" %in% names(df) && !is.factor(df$age_grp))
    df$age_grp <- factor(df$age_grp)
  if ("age_group" %in% names(df) && !is.factor(df$age_group))
    df$age_group <- factor(df$age_group)
  if ("condition" %in% names(df) && !is.factor(df$condition))
    df$condition <- factor(df$condition)
  df
}

# Rename age_group -> age_grp for consistency
fix_age_col <- function(df) {
  if ("age_group" %in% names(df) && !"age_grp" %in% names(df))
    df <- rename(df, age_grp = age_group)
  df
}

# Extract ANOVA table as tidy character block
anova_summary <- function(model, label) {
  a <- as.data.frame(anova(model))
  cat("\n", strrep("=", 70), "\n")
  cat("MODEL:", label, "\n")
  cat(strrep("=", 70), "\n")
  for (i in seq_len(nrow(a))) {
    term <- rownames(a)[i]
    cat(sprintf("  %-35s F(%g, %5.1f) = %5.2f,  p = %.3f%s\n",
                term,
                a[i, "NumDF"],
                a[i, "DenDF"],
                a[i, "F value"],
                a[i, "Pr(>F)"],
                ifelse(a[i, "Pr(>F)"] < .05, " *", "")))
  }
}

# Absolute and % change summary: pre vs post, by age_grp x condition
change_summary <- function(df, outcome_col, group_cols = c("age_grp", "condition"),
                           label = "") {
  cat("\n--- Change summary:", label, "---\n")

  # Pre → Post
  pre_post <- df %>%
    filter(time %in% c("pre", "post")) %>%
    group_by(across(all_of(c("participant", group_cols)))) %>%
    summarise(
      pre  = mean(.data[[outcome_col]][time == "pre"],  na.rm = TRUE),
      post = mean(.data[[outcome_col]][time == "post"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(abs_change = post - pre,
           pct_change = (post - pre) / pre * 100)

  pp_summary <- pre_post %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      pre_mean   = mean(pre,        na.rm = TRUE),
      post_mean  = mean(post,       na.rm = TRUE),
      abs_mean   = mean(abs_change, na.rm = TRUE),
      abs_sd     = sd(abs_change,   na.rm = TRUE),
      pct_mean   = mean(pct_change, na.rm = TRUE),
      pct_sd     = sd(pct_change,   na.rm = TRUE),
      .groups = "drop"
    )

  cat("  Pre → Post (mean ± SD):\n")
  for (i in seq_len(nrow(pp_summary))) {
    r <- pp_summary[i, ]
    grp <- paste(unlist(r[group_cols]), collapse = " / ")
    cat(sprintf("    %-25s  Δ = %+.2f ± %.2f  (%+.1f%% ± %.1f%%)\n",
                grp, r$abs_mean, r$abs_sd, r$pct_mean, r$pct_sd))
  }

  # Low vs Mod at post
  if ("condition" %in% group_cols) {
    age_col <- intersect(group_cols, c("age_grp", "age_group"))
    lv_mod <- pre_post %>%
      select(participant, all_of(age_col), condition, post) %>%
      pivot_wider(names_from = condition, values_from = post) %>%
      mutate(low_vs_mod = low - mod,
             low_vs_mod_pct = (low - mod) / mod * 100)

    lvm_summary <- lv_mod %>%
      group_by(across(all_of(age_col))) %>%
      summarise(
        diff_mean = mean(low_vs_mod,     na.rm = TRUE),
        diff_sd   = sd(low_vs_mod,       na.rm = TRUE),
        pct_mean  = mean(low_vs_mod_pct, na.rm = TRUE),
        pct_sd    = sd(low_vs_mod_pct,   na.rm = TRUE),
        .groups = "drop"
      )

    cat("  Low vs Mod at post (low minus mod, mean ± SD):\n")
    for (i in seq_len(nrow(lvm_summary))) {
      r <- lvm_summary[i, ]
      grp <- paste(unlist(r[age_col]), collapse = " / ")
      cat(sprintf("    %-15s  Δ = %+.2f ± %.2f  (%+.1f%% ± %.1f%%)\n",
                  grp, r$diff_mean, r$diff_sd, r$pct_mean, r$pct_sd))
    }
  }
}

# Control comparison change summary (trained vs untrained, old only)
change_summary_ctrl <- function(df, outcome_col, trained_col = "trained", label = "") {
  cat("\n--- Control change summary:", label, "---\n")

  pre_post <- df %>%
    filter(time %in% c("pre", "post")) %>%
    group_by(participant, .data[[trained_col]]) %>%
    summarise(
      pre  = mean(.data[[outcome_col]][time == "pre"],  na.rm = TRUE),
      post = mean(.data[[outcome_col]][time == "post"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(abs_change = post - pre,
           pct_change = (post - pre) / pre * 100)

  pp_summary <- pre_post %>%
    group_by(.data[[trained_col]]) %>%
    summarise(
      abs_mean = mean(abs_change, na.rm = TRUE),
      abs_sd   = sd(abs_change,   na.rm = TRUE),
      pct_mean = mean(pct_change, na.rm = TRUE),
      pct_sd   = sd(pct_change,   na.rm = TRUE),
      .groups = "drop"
    )

  for (i in seq_len(nrow(pp_summary))) {
    r <- pp_summary[i, ]
    cat(sprintf("    %-15s  Δ = %+.2f ± %.2f  (%+.1f%% ± %.1f%%)\n",
                r[[trained_col]], r$abs_mean, r$abs_sd, r$pct_mean, r$pct_sd))
  }
}

# =============================================================================
# PREPARE DATASETS
# =============================================================================

# Muscle volume: two timepoints, age_group -> age_grp
muscle_volume <- muscle_volume %>%
  fix_age_col() %>%
  prep_factors(time_levels = c("pre", "post")) %>%
  filter(allocation == "int")   # intervention only for primary model

# Muscle volume control dataset (old only, trained vs control)
mv_con <- readRDS("./data/data-gen/muscle_volume.RDS") %>%
  fix_age_col() %>%
  prep_factors(time_levels = c("pre", "post")) %>%
  filter(age_grp == "old") %>%
  mutate(trained = ifelse(allocation == "int", "trained", "control"),
         trained = factor(trained))

# Muscle thickness
muscle_thickness <- muscle_thickness %>%
  prep_factors(time_levels = c("pre", "mid", "post"))

# Muscle thickness control dataset
mt_con <- muscle_thickness %>%
  filter(age_grp == "old") %>%
  mutate(trained = ifelse(allocation == "int", "trained", "control"),
         trained = factor(trained))

# Lean mass
lean_dat <- lean_dat %>%
  fix_age_col() %>%
  prep_factors(time_levels = c("pre", "mid", "post")) %>%
  filter(allocation == "int")

lean_con <- readRDS("./data/data-gen/lean_dat.rds") %>%
  fix_age_col() %>%
  prep_factors(time_levels = c("pre", "mid", "post")) %>%
  filter(age_grp == "old") %>%
  mutate(trained = ifelse(allocation == "int", "trained", "control"),
         trained = factor(trained))

# Isokinetic: split by speed
iso_isok <- iso_isok %>%
  prep_factors(time_levels = c("pre", "mid", "post")) %>%
  mutate(speed = as.character(speed))

# Isometric
iso_isom   <- prep_factors(iso_isom,   time_levels = c("pre", "mid", "post"))
isom_int   <- prep_factors(isom_int,   time_levels = c("pre", "mid", "post"))
isom_con   <- prep_factors(isom_con,   time_levels = c("pre", "post"))

# Isokinetic speed subsets
pt_60_int  <- prep_factors(pt_60_int,  time_levels = c("pre", "mid", "post"))
pt_60_con  <- prep_factors(pt_60_con,  time_levels = c("pre", "post"))
pt_120_int <- prep_factors(pt_120_int, time_levels = c("pre", "mid", "post"))
pt_120_con <- prep_factors(pt_120_con, time_levels = c("pre", "post"))
pt_240_int <- prep_factors(pt_240_int, time_levels = c("pre", "mid", "post"))
pt_240_con <- prep_factors(pt_240_con, time_levels = c("pre", "post"))

# Keiser
keiser_dat <- prep_factors(keiser_dat, time_levels = c("pre", "mid", "post"))
keiser_int <- prep_factors(keiser_int, time_levels = c("pre", "mid", "post"))
keiser_con <- prep_factors(keiser_con, time_levels = c("pre", "post"))

# Bicep curl — fix 3021 outlier
bc_dat <- bc_dat %>%
  prep_factors(time_levels = c("pre", "mid", "post")) %>%
  mutate(mvc = ifelse(participant == "3021" & mvc > 500, NA_real_, mvc))
bc_int <- bc_int %>%
  prep_factors(time_levels = c("pre", "mid", "post")) %>%
  mutate(mvc = ifelse(participant == "3021" & mvc > 500, NA_real_, mvc))
bc_con <- bc_con %>%
  prep_factors(time_levels = c("pre", "post")) %>%
  mutate(mvc = ifelse(participant == "3021" & mvc > 500, NA_real_, mvc))

# =============================================================================
# FIT MODELS — helper to try nested random effect, fall back to simple
# =============================================================================

fit_lmm <- function(formula_nested, formula_simple, data) {
  m <- tryCatch(
    suppressMessages(lmer(formula_nested, data = data, REML = FALSE,
                          control = lmerControl(optimizer = "bobyqa"))),
    error = function(e) NULL
  )
  if (is.null(m) || isSingular(m)) {
    m <- lmer(formula_simple, data = data, REML = FALSE,
              control = lmerControl(optimizer = "bobyqa"))
  }
  m
}

# =============================================================================
# 0. EXERCISE VOLUME LOAD
# =============================================================================

exercise_dat <- readRDS("./data/data-gen/exercise_volume.rds") %>%
  mutate(age_grp  = factor(age_grp),
         condition = factor(condition))

mod_vl <- lmer(
  log_vl ~ age_grp * condition + (1 | participant),
  data = exercise_dat, REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

anova_summary(mod_vl, "Exercise Volume Load — log(total VL) ~ age_grp * condition")

# Raw volume summary: mean ± SD per age_grp × condition
cat("\n--- Volume load summary (raw kg, mean ± SD) ---\n")
vl_summary <- exercise_dat %>%
  group_by(age_grp, condition) %>%
  summarise(
    mean_vl = mean(total_vl, na.rm = TRUE),
    sd_vl   = sd(total_vl,   na.rm = TRUE),
    .groups = "drop"
  )
for (i in seq_len(nrow(vl_summary))) {
  r <- vl_summary[i, ]
  cat(sprintf("    %-10s %-5s  %.1f ± %.1f kg\n",
              r$age_grp, r$condition, r$mean_vl, r$sd_vl))
}

# Mod/low ratio per age group
cat("\n--- Mod / Low ratio (mean total VL) ---\n")
vl_ratio <- vl_summary %>%
  pivot_wider(names_from = condition, values_from = c(mean_vl, sd_vl)) %>%
  mutate(ratio = mean_vl_mod / mean_vl_low)
for (i in seq_len(nrow(vl_ratio))) {
  r <- vl_ratio[i, ]
  cat(sprintf("    %-10s  mod/low = %.2fx\n", r$age_grp, r$ratio))
}

# =============================================================================
# 1. MUSCLE VOLUME
# =============================================================================

mod_mv <- fit_lmm(
  volume ~ age_grp * condition * time + (1 | participant/leg),
  volume ~ age_grp * condition * time + (1 | participant),
  data = muscle_volume
)

mod_mv_con <- fit_lmm(
  volume ~ trained * time + (1 | participant/leg),
  volume ~ trained * time + (1 | participant),
  data = mv_con
)

anova_summary(mod_mv,     "Muscle Volume — Intervention (age_grp × condition × time)")
anova_summary(mod_mv_con, "Muscle Volume — Old: Trained vs Control")

change_summary(muscle_volume, "volume",
               group_cols = c("age_grp", "condition"),
               label = "Muscle Volume")
change_summary_ctrl(mv_con, "volume", label = "Muscle Volume — Old ctrl")

# =============================================================================
# 2. MUSCLE THICKNESS
# =============================================================================

mod_mt <- fit_lmm(
  thickness ~ age_grp * condition * time + (1 | participant/leg),
  thickness ~ age_grp * condition * time + (1 | participant),
  data = filter(muscle_thickness, allocation == "int")
)

mod_mt_con <- fit_lmm(
  thickness ~ trained * time + (1 | participant/leg),
  thickness ~ trained * time + (1 | participant),
  data = mt_con
)

anova_summary(mod_mt,     "Muscle Thickness — Intervention")
anova_summary(mod_mt_con, "Muscle Thickness — Old: Trained vs Control")

change_summary(filter(muscle_thickness, allocation == "int"), "thickness",
               group_cols = c("age_grp", "condition"),
               label = "Muscle Thickness")
change_summary_ctrl(mt_con, "thickness", label = "Muscle Thickness — Old ctrl")

# =============================================================================
# 3. ARM LEAN MASS
# =============================================================================

mod_alm <- fit_lmm(
  kg ~ age_grp * condition * time + (1 | participant/side),
  kg ~ age_grp * condition * time + (1 | participant),
  data = lean_dat
)

mod_alm_con <- fit_lmm(
  kg ~ trained * time + (1 | participant/side),
  kg ~ trained * time + (1 | participant),
  data = lean_con
)

anova_summary(mod_alm,     "Arm Lean Mass — Intervention")
anova_summary(mod_alm_con, "Arm Lean Mass — Old: Trained vs Control")

change_summary(lean_dat, "kg",
               group_cols = c("age_grp", "condition"),
               label = "Arm Lean Mass")
change_summary_ctrl(lean_con, "kg", label = "Arm Lean Mass — Old ctrl")

# =============================================================================
# 4. ISOMETRIC MVC
# =============================================================================

# Aggregate to mean per participant/leg/time before modelling
isom_agg <- isom_int %>%
  group_by(participant, leg, time, age_grp, condition, allocation) %>%
  summarise(pt = mean(pt, na.rm = TRUE), .groups = "drop")

isom_con_agg <- isom_con %>%
  group_by(participant, leg, time, age_grp, trained) %>%
  summarise(pt = mean(pt, na.rm = TRUE), .groups = "drop")

mod_isom <- fit_lmm(
  pt ~ age_grp * condition * time + (1 | participant/leg),
  pt ~ age_grp * condition * time + (1 | participant),
  data = isom_agg
)

mod_isom_con <- fit_lmm(
  pt ~ trained * time + (1 | participant/leg),
  pt ~ trained * time + (1 | participant),
  data = isom_con_agg
)

anova_summary(mod_isom,     "Isometric MVC — Intervention")
anova_summary(mod_isom_con, "Isometric MVC — Old: Trained vs Control")

change_summary(isom_agg, "pt",
               group_cols = c("age_grp", "condition"),
               label = "Isometric MVC")
change_summary_ctrl(isom_con_agg, "pt", label = "Isometric MVC — Old ctrl")

# =============================================================================
# 5. ISOKINETIC 60°/s
# =============================================================================

isok60_agg <- pt_60_int %>%
  group_by(participant, leg, time, age_grp, condition) %>%
  summarise(pt = mean(pt, na.rm = TRUE), .groups = "drop")

isok60_con_agg <- pt_60_con %>%
  group_by(participant, leg, time, age_grp, trained) %>%
  summarise(pt = mean(pt, na.rm = TRUE), .groups = "drop")

mod_isok60 <- fit_lmm(
  pt ~ age_grp * condition * time + (1 | participant/leg),
  pt ~ age_grp * condition * time + (1 | participant),
  data = isok60_agg
)

mod_isok60_con <- fit_lmm(
  pt ~ trained * time + (1 | participant/leg),
  pt ~ trained * time + (1 | participant),
  data = isok60_con_agg
)

anova_summary(mod_isok60,     "Isokinetic 60°/s — Intervention")
anova_summary(mod_isok60_con, "Isokinetic 60°/s — Old: Trained vs Control")

change_summary(isok60_agg, "pt",
               group_cols = c("age_grp", "condition"),
               label = "Isokinetic 60°/s")
change_summary_ctrl(isok60_con_agg, "pt", label = "Isokinetic 60°/s — Old ctrl")

# =============================================================================
# 6. ISOKINETIC 120°/s
# =============================================================================

isok120_agg <- pt_120_int %>%
  group_by(participant, leg, time, age_grp, condition) %>%
  summarise(pt = mean(pt, na.rm = TRUE), .groups = "drop")

isok120_con_agg <- pt_120_con %>%
  group_by(participant, leg, time, age_grp, trained) %>%
  summarise(pt = mean(pt, na.rm = TRUE), .groups = "drop")

mod_isok120 <- fit_lmm(
  pt ~ age_grp * condition * time + (1 | participant/leg),
  pt ~ age_grp * condition * time + (1 | participant),
  data = isok120_agg
)

mod_isok120_con <- fit_lmm(
  pt ~ trained * time + (1 | participant/leg),
  pt ~ trained * time + (1 | participant),
  data = isok120_con_agg
)

anova_summary(mod_isok120,     "Isokinetic 120°/s — Intervention")
anova_summary(mod_isok120_con, "Isokinetic 120°/s — Old: Trained vs Control")

change_summary(isok120_agg, "pt",
               group_cols = c("age_grp", "condition"),
               label = "Isokinetic 120°/s")
change_summary_ctrl(isok120_con_agg, "pt", label = "Isokinetic 120°/s — Old ctrl")

# =============================================================================
# 7. ISOKINETIC 240°/s
# =============================================================================

isok240_agg <- pt_240_int %>%
  group_by(participant, leg, time, age_grp, condition) %>%
  summarise(pt = mean(pt, na.rm = TRUE), .groups = "drop")

isok240_con_agg <- pt_240_con %>%
  group_by(participant, leg, time, age_grp, trained) %>%
  summarise(pt = mean(pt, na.rm = TRUE), .groups = "drop")

mod_isok240 <- fit_lmm(
  pt ~ age_grp * condition * time + (1 | participant/leg),
  pt ~ age_grp * condition * time + (1 | participant),
  data = isok240_agg
)

mod_isok240_con <- fit_lmm(
  pt ~ trained * time + (1 | participant/leg),
  pt ~ trained * time + (1 | participant),
  data = isok240_con_agg
)

anova_summary(mod_isok240,     "Isokinetic 240°/s — Intervention")
anova_summary(mod_isok240_con, "Isokinetic 240°/s — Old: Trained vs Control")

change_summary(isok240_agg, "pt",
               group_cols = c("age_grp", "condition"),
               label = "Isokinetic 240°/s")
change_summary_ctrl(isok240_con_agg, "pt", label = "Isokinetic 240°/s — Old ctrl")

# =============================================================================
# 8. KEISER 1RM LEG PRESS
# =============================================================================

keiser_agg <- keiser_int %>%
  group_by(participant, leg, time, age_grp, condition) %>%
  summarise(maxrm = mean(maxrm, na.rm = TRUE), .groups = "drop")

keiser_con_agg <- keiser_con %>%
  group_by(participant, leg, time, age_grp, trained) %>%
  summarise(maxrm = mean(maxrm, na.rm = TRUE), .groups = "drop")

mod_keiser <- lmer(
  maxrm ~ age_grp * condition * time + (1 | participant),
  data = keiser_agg, REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

mod_keiser_con <- lmer(
  maxrm ~ trained * time + (1 | participant),
  data = keiser_con_agg, REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

anova_summary(mod_keiser,     "Keiser 1RM Leg Press — Intervention")
anova_summary(mod_keiser_con, "Keiser 1RM — Old: Trained vs Control")

change_summary(keiser_agg, "maxrm",
               group_cols = c("age_grp", "condition"),
               label = "Keiser 1RM")
change_summary_ctrl(keiser_con_agg, "maxrm", label = "Keiser 1RM — Old ctrl")

# =============================================================================
# 9. BICEP CURL MVC
# =============================================================================

bc_agg <- bc_int %>%
  mutate(mvc = if_else(participant == "3021" & arm == "R" & time == "pre", NA_real_, mvc)) %>%
  group_by(participant, arm, time, age_grp, condition) %>%
  summarise(mvc = mean(mvc, na.rm = TRUE), .groups = "drop")

bc_con_agg <- bc_con %>%
  mutate(mvc = if_else(participant == "3021" & arm == "R" & time == "pre", NA_real_, mvc)) %>%
  group_by(participant, arm, time, age_grp, trained) %>%
  summarise(mvc = mean(mvc, na.rm = TRUE), .groups = "drop")

mod_bc <- lmer(
  mvc ~ age_grp * condition * time + (1 | participant),
  data = bc_agg, REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

mod_bc_con <- lmer(
  mvc ~ trained * time + (1 | participant),
  data = bc_con_agg, REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

anova_summary(mod_bc,     "Bicep Curl MVC — Intervention")
anova_summary(mod_bc_con, "Bicep Curl MVC — Old: Trained vs Control")

change_summary(bc_agg, "mvc",
               group_cols = c("age_grp", "condition"),
               label = "Bicep Curl MVC")
change_summary_ctrl(bc_con_agg, "mvc", label = "Bicep Curl MVC — Old ctrl")

# =============================================================================
# DONE
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("All models complete.\n")
cat(strrep("=", 70), "\n")
