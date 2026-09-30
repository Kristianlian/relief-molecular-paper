# ==============================================================================
# BODY COMPOSITION ANALYSIS - LINEAR MIXED MODELS
# Research Question: Does age moderate the response to training volume?
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. LOAD REQUIRED PACKAGES
# ------------------------------------------------------------------------------

# Install packages if needed (run once):
# install.packages(c("tidyverse", "lme4", "lmerTest", "emmeans", "ggplot2", "car"))

library(tidyverse)   # Data manipulation and visualization
library(lme4)        # Linear mixed models
library(lmerTest)    # P-values for mixed models
library(emmeans)     # Estimated marginal means and post-hoc tests
library(car)         # For Anova() function with Type III SS
library(broom.mixed) # Tidy model outputs

# ------------------------------------------------------------------------------
# 2. LOAD AND INSPECT DATA
# ------------------------------------------------------------------------------

# Load your data (adjust path as needed)
# data <- read.csv("your_data.csv")

us.dat <- reliefdata::relief_thickness
dxa.dat <- reliefdata::relief_bodycomp
mr.dat <- reliefdata::relief_mr
condition <- reliefdata::relief_volume %>%
  mutate(participant = as.character(participant))
participants <- reliefdata::relief_participants %>%
  select(participant, sex, age)

thick.dat <- us.dat %>%
  full_join(condition) %>%
  full_join(participants) %>%
  print()

# Expected structure:
# ID | Age | Sex | BMI | Time | Volume | VL | VI | Thigh_volume | Limb_lean_mass | ...

# Check data structure
str(thick.dat)
head(thick.dat)
summary(thick.dat)

# Check for missing values
colSums(is.na(thick.dat))

# ------------------------------------------------------------------------------
# 3. DATA PREPARATION
# ------------------------------------------------------------------------------

# 3.1 Create composite muscle thickness (VL + VI average)
comp.dat <- thick.dat %>%
  mutate(
    composite_thickness = (VLthick + VIthick) / 2
  )

# 3.2 Create limb-specific baseline values
# These will be constant for each leg (Volume condition) within each person
composite.dat <- comp.dat %>%
  group_by(participant, condition) %>%
  mutate(
    # Baseline values for muscle thickness
    baseline_VL = mean(VLthick[time == "pre"]),
    baseline_VI = mean(VIthick[time == "pre"]),
    baseline_composite = mean(composite_thickness[time == "pre"])) %>%
  ungroup()

# 3.3 Ensure proper factor coding with correct reference levels
factor.dat <- composite.dat %>%
  mutate(
    # Age: Young as reference
    age.grp = factor(age.grp, levels = c("yng", "old")),
    
    # Volume: Low as reference
    condition = factor(condition, levels = c("low", "mod")),
    
    # Time: Baseline as reference (will be implicit in models with baseline covariate)
    time = factor(time, levels = c("pre", "mid", "post")),
    
    allocation = factor(allocation, levels = c("int", "con")),
    
    # Sex as factor
    sex = factor(sex),
    
    # ID as factor for random effects
    participant = factor(participant)
  )

# 3.4 Check factor levels
levels(factor.dat$age.grp)
levels(factor.dat$condition)
levels(factor.dat$time)
levels(factor.dat$allocation)

# 3.5 Verify baseline values are constant within person-limb combinations
factor.dat %>%
  group_by(participant, condition) %>%
  summarise(
    baseline_var = var(baseline_composite, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  summary()
# baseline_var should be 0 or very close to 0

# ------------------------------------------------------------------------------
# 4. DESCRIPTIVE STATISTICS
# ------------------------------------------------------------------------------

# 4.1 Sample characteristics at baseline
baseline.dat <- factor.dat %>%
  filter(time == "pre", condition == "low") %>%  # One row per person
  group_by(age.grp) %>%
  summarise(
    n = n(),
    age_mean = mean(age, na.rm = TRUE),  # If you have actual age variable
    age_sd = sd(age, na.rm = TRUE),
  #  bmi_mean = mean(BMI, na.rm = TRUE),
  #  bmi_sd = sd(BMI, na.rm = TRUE),
    sex_male_n = sum(sex == "male", na.rm = TRUE),
    .groups = "drop"
  )

print(baseline.dat)

# 4.2 Outcome variables at each time point
desc.dat <- factor.dat %>%
  group_by(age.grp, condition, time, allocation) %>%
  summarise(
    n = n(),
    composite_mean = mean(composite_thickness, na.rm = TRUE),
    composite_sd = sd(composite_thickness, na.rm = TRUE),
#    thigh_vol_mean = mean(Thigh_volume, na.rm = TRUE), # MRI data not included here
#    thigh_vol_sd = sd(Thigh_volume, na.rm = TRUE),
#    limb_lean_mean = mean(Limb_lean_mass, na.rm = TRUE), # DXA data not included here
#    limb_lean_sd = sd(Limb_lean_mass, na.rm = TRUE),
    .groups = "drop"
  )

print(desc.dat) # I think I need to add a separate description for the control
# group within the condition variable, because they should not be group by 
# low and/or mod since they did not exercise. Perhaps just add "non" or so.

# ------------------------------------------------------------------------------
# 5. PRIMARY ANALYSIS 1: COMPOSITE MUSCLE THICKNESS (VL + VI)
# ------------------------------------------------------------------------------

cat("\n=== ANALYSIS 1: COMPOSITE MUSCLE THICKNESS ===\n")

# 5.1 Fit the linear mixed model
# Model: Outcome ~ Age × Volume × Time + Baseline + Random intercept per person
model_thickness <- lmer(
  Composite_thickness ~ Age * Volume * Time + Baseline_composite + (1|ID),
  data = data,
  REML = FALSE,  # Use ML for model comparison; REML for final estimates
  control = lmerControl(optimizer = "bobyqa")  # Helps with convergence
)

# 5.2 Check model convergence and singular fit warnings
summary(model_thickness)
# Look for convergence warnings; if present, may need to simplify model

# 5.3 Model assumptions
# Residual plots
plot(model_thickness, 
     main = "Residuals vs Fitted - Muscle Thickness",
     xlab = "Fitted values", 
     ylab = "Residuals")

# Q-Q plot for normality of residuals
qqnorm(resid(model_thickness), main = "Q-Q Plot - Muscle Thickness")
qqline(resid(model_thickness))

# Histogram of residuals
hist(resid(model_thickness), breaks = 30, 
     main = "Distribution of Residuals - Muscle Thickness",
     xlab = "Residuals")

# 5.4 Main results - Type III ANOVA
# This tests each main effect and interaction
anova_thickness <- anova(model_thickness, type = 3)
print(anova_thickness)

# Alternative using car package (sometimes preferred)
anova_thickness_car <- Anova(model_thickness, type = 3)
print(anova_thickness_car)

# 5.5 Model summary with coefficient estimates
summary(model_thickness)

# 5.6 Post-hoc tests IF Age × Volume × Time interaction is significant
# Check p-value from ANOVA first
if (anova_thickness$`Pr(>F)`[which(rownames(anova_thickness) == "Age:Volume:Time")] < 0.05) {
  
  cat("\n*** Three-way interaction is significant! Decomposing... ***\n")
  
  # Estimated marginal means
  emm_thickness <- emmeans(model_thickness, ~ Age | Volume | Time)
  print(emm_thickness)
  
  # Pairwise comparisons within each Age × Volume combination across Time
  pairs_thickness <- pairs(emm_thickness, adjust = "bonferroni")
  print(pairs_thickness)
  
  # Alternative: Compare Volume within each Age × Time combination
  emm_thickness_vol <- emmeans(model_thickness, ~ Volume | Age | Time)
  pairs_thickness_vol <- pairs(emm_thickness_vol, adjust = "bonferroni")
  print(pairs_thickness_vol)
  
} else {
  cat("\n*** Three-way interaction not significant. Check two-way interactions. ***\n")
  
  # If two-way interactions are significant, decompose those
  # Example: Age × Volume
  emm_age_vol <- emmeans(model_thickness, ~ Age | Volume)
  print(emm_age_vol)
}

# 5.7 Effect sizes
# R-squared (marginal = fixed effects only, conditional = fixed + random)
# Requires MuMIn package
library(MuMIn)
r.squaredGLMM(model_thickness)

# ------------------------------------------------------------------------------
# 6. PRIMARY ANALYSIS 2: MUSCLE VOLUME (THIGH MRI)
# ------------------------------------------------------------------------------

cat("\n=== ANALYSIS 2: MUSCLE VOLUME (THIGH MRI) ===\n")

# Note: Only Baseline and Post measurements available
# Filter to these time points only
data_volume <- data %>%
  filter(Time %in% c("Baseline", "Post"))

# 6.1 Fit the model
model_volume <- lmer(
  Thigh_volume ~ Age * Volume * Time + Baseline_thigh_volume + (1|ID),
  data = data_volume,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

# 6.2 Check convergence
summary(model_volume)

# 6.3 Model assumptions
plot(model_volume, main = "Residuals vs Fitted - Muscle Volume")
qqnorm(resid(model_volume), main = "Q-Q Plot - Muscle Volume")
qqline(resid(model_volume))

# 6.4 Main results
anova_volume <- anova(model_volume, type = 3)
print(anova_volume)

# 6.5 Model summary
summary(model_volume)

# 6.6 Post-hoc tests if interactions are significant
# Follow similar logic as muscle thickness above
emm_volume <- emmeans(model_volume, ~ Age | Volume | Time)
print(emm_volume)

# 6.7 Effect sizes
r.squaredGLMM(model_volume)

# ------------------------------------------------------------------------------
# 7. PRIMARY ANALYSIS 3: LIMB LEAN MASS (DXA)
# ------------------------------------------------------------------------------

cat("\n=== ANALYSIS 3: LIMB LEAN MASS (DXA) ===\n")

# 7.1 Fit the model
model_lean_mass <- lmer(
  Limb_lean_mass ~ Age * Volume * Time + Baseline_limb_lean + (1|ID),
  data = data,
  REML = FALSE,
  control = lmerControl(optimizer = "bobyqa")
)

# 7.2 Check convergence
summary(model_lean_mass)

# 7.3 Model assumptions
plot(model_lean_mass, main = "Residuals vs Fitted - Limb Lean Mass")
qqnorm(resid(model_lean_mass), main = "Q-Q Plot - Limb Lean Mass")
qqline(resid(model_lean_mass))

# 7.4 Main results
anova_lean_mass <- anova(model_lean_mass, type = 3)
print(anova_lean_mass)

# 7.5 Model summary
summary(model_lean_mass)

# 7.6 Post-hoc tests
emm_lean_mass <- emmeans(model_lean_mass, ~ Age | Volume | Time)
print(emm_lean_mass)

# 7.7 Effect sizes
r.squaredGLMM(model_lean_mass)

# ------------------------------------------------------------------------------
# 8. SECONDARY ANALYSES: INDIVIDUAL MUSCLES (VL and VI)
# ------------------------------------------------------------------------------

cat("\n=== SECONDARY ANALYSIS: VASTUS LATERALIS ===\n")

# VL model
model_VL <- lmer(
  VL ~ Age * Volume * Time + Baseline_VL + (1|ID),
  data = data,
  REML = FALSE
)

anova_VL <- anova(model_VL, type = 3)
print(anova_VL)
summary(model_VL)

cat("\n=== SECONDARY ANALYSIS: VASTUS INTERMEDIUS ===\n")

# VI model
model_VI <- lmer(
  VI ~ Age * Volume * Time + Baseline_VI + (1|ID),
  data = data,
  REML = FALSE
)

anova_VI <- anova(model_VI, type = 3)
print(anova_VI)
summary(model_VI)

# ------------------------------------------------------------------------------
# 9. MULTIPLE COMPARISONS CORRECTION
# ------------------------------------------------------------------------------

# You have 3 primary outcomes: Composite thickness, Muscle volume, Limb lean mass
# Apply Bonferroni correction: α = 0.05 / 3 = 0.017

alpha_corrected <- 0.05 / 3
cat(sprintf("\nCorrected alpha level (Bonferroni): %.4f\n", alpha_corrected))

# Extract p-values for Age × Volume × Time interaction from each model
p_vals <- c(
  Thickness = anova_thickness$`Pr(>F)`[which(rownames(anova_thickness) == "Age:Volume:Time")],
  Volume = anova_volume$`Pr(>F)`[which(rownames(anova_volume) == "Age:Volume:Time")],
  Lean_mass = anova_lean_mass$`Pr(>F)`[which(rownames(anova_lean_mass) == "Age:Volume:Time")]
)

# Check which remain significant after correction
significant_after_correction <- p_vals < alpha_corrected
print(p_vals)
print(significant_after_correction)

# ------------------------------------------------------------------------------
# 10. VISUALIZATIONS
# ------------------------------------------------------------------------------

# 10.1 Trajectory plot for muscle thickness
plot_thickness <- data %>%
  group_by(Age, Volume, Time) %>%
  summarise(
    mean = mean(Composite_thickness, na.rm = TRUE),
    se = sd(Composite_thickness, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Time, y = mean, color = Volume, group = Volume)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  facet_wrap(~ Age) +
  labs(
    title = "Muscle Thickness Over Time",
    subtitle = "By Age Group and Training Volume",
    x = "Time Point",
    y = "Composite Muscle Thickness (cm)",
    color = "Training Volume"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

print(plot_thickness)

# Save plot
ggsave("muscle_thickness_trajectory.png", plot_thickness, 
       width = 8, height = 6, dpi = 300)

# 10.2 Trajectory plot for muscle volume
plot_volume <- data_volume %>%
  group_by(Age, Volume, Time) %>%
  summarise(
    mean = mean(Thigh_volume, na.rm = TRUE),
    se = sd(Thigh_volume, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Time, y = mean, color = Volume, group = Volume)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  facet_wrap(~ Age) +
  labs(
    title = "Muscle Volume Over Time",
    subtitle = "By Age Group and Training Volume",
    x = "Time Point",
    y = "Thigh Muscle Volume (cm³)",
    color = "Training Volume"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.position = "bottom"
  )

print(plot_volume)
ggsave("muscle_volume_trajectory.png", plot_volume, 
       width = 8, height = 6, dpi = 300)

# 10.3 Individual trajectories (spaghetti plot) - useful for checking outliers
spaghetti_plot <- ggplot(data, 
                         aes(x = Time, y = Composite_thickness, 
                             group = interaction(ID, Volume), 
                             color = Age)) +
  geom_line(alpha = 0.3) +
  facet_grid(Age ~ Volume) +
  stat_summary(aes(group = Age), fun = mean, geom = "line", 
               size = 2, color = "black") +
  labs(
    title = "Individual Trajectories - Muscle Thickness",
    x = "Time Point",
    y = "Composite Muscle Thickness (cm)"
  ) +
  theme_minimal()

print(spaghetti_plot)

# ------------------------------------------------------------------------------
# 11. SENSITIVITY ANALYSES
# ------------------------------------------------------------------------------

cat("\n=== SENSITIVITY ANALYSIS: Adding Sex and BMI ===\n")

# Refit primary model with additional covariates
model_thickness_sensitivity <- lmer(
  Composite_thickness ~ Age * Volume * Time + Baseline_composite + 
    Sex + BMI + (1|ID),
  data = data,
  REML = FALSE
)

anova_thickness_sensitivity <- anova(model_thickness_sensitivity, type = 3)
print(anova_thickness_sensitivity)

# Compare with primary model
cat("\nComparing primary model to model with Sex and BMI:\n")
anova(model_thickness, model_thickness_sensitivity)

# Check if Age × Volume × Time interaction changes
cat("\nPrimary model interaction p-value:", 
    anova_thickness$`Pr(>F)`[which(rownames(anova_thickness) == "Age:Volume:Time")], "\n")
cat("Sensitivity model interaction p-value:", 
    anova_thickness_sensitivity$`Pr(>F)`[which(rownames(anova_thickness_sensitivity) == "Age:Volume:Time")], "\n")

# ------------------------------------------------------------------------------
# 12. EXPORT RESULTS
# ------------------------------------------------------------------------------

# Create a summary table of main results
results_summary <- data.frame(
  Outcome = c("Composite Thickness", "Muscle Volume", "Limb Lean Mass"),
  Age_effect = c(
    anova_thickness$`Pr(>F)`[which(rownames(anova_thickness) == "Age")],
    anova_volume$`Pr(>F)`[which(rownames(anova_volume) == "Age")],
    anova_lean_mass$`Pr(>F)`[which(rownames(anova_lean_mass) == "Age")]
  ),
  Volume_effect = c(
    anova_thickness$`Pr(>F)`[which(rownames(anova_thickness) == "Volume")],
    anova_volume$`Pr(>F)`[which(rownames(anova_volume) == "Volume")],
    anova_lean_mass$`Pr(>F)`[which(rownames(anova_lean_mass) == "Volume")]
  ),
  Time_effect = c(
    anova_thickness$`Pr(>F)`[which(rownames(anova_thickness) == "Time")],
    anova_volume$`Pr(>F)`[which(rownames(anova_volume) == "Time")],
    anova_lean_mass$`Pr(>F)`[which(rownames(anova_lean_mass) == "Time")]
  ),
  Age_x_Volume_x_Time = c(
    anova_thickness$`Pr(>F)`[which(rownames(anova_thickness) == "Age:Volume:Time")],
    anova_volume$`Pr(>F)`[which(rownames(anova_volume) == "Age:Volume:Time")],
    anova_lean_mass$`Pr(>F)`[which(rownames(anova_lean_mass) == "Age:Volume:Time")]
  )
)

print(results_summary)

# Export to CSV
write.csv(results_summary, "analysis_results_summary.csv", row.names = FALSE)

# Export detailed model outputs
sink("model_outputs.txt")
cat("=== COMPOSITE MUSCLE THICKNESS MODEL ===\n")
print(summary(model_thickness))
print(anova_thickness)
cat("\n=== MUSCLE VOLUME MODEL ===\n")
print(summary(model_volume))
print(anova_volume)
cat("\n=== LIMB LEAN MASS MODEL ===\n")
print(summary(model_lean_mass))
print(anova_lean_mass)
sink()

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results exported to:\n")
cat("  - analysis_results_summary.csv\n")
cat("  - model_outputs.txt\n")
cat("  - muscle_thickness_trajectory.png\n")
cat("  - muscle_volume_trajectory.png\n")