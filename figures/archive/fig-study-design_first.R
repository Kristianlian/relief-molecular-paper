## Study design + Exercise Figure

#' ---
#' title: Figure 1 (study design and weekly training volume)
#' format: html
#' editor_options:
#'   chunk_output_type: console
#' ---

library(here)
library(tidyverse)
library(cowplot)
library(readxl)   
library(png)      

# ── Shared aesthetics ────────────────────────────────────────────────────────
col_yng <- "#2166ac"
col_old <- "#d6604d"
col_con <- "#4d4d4d"

base_theme <- theme_minimal(base_size = 8) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.3),
    axis.ticks = element_line(colour = "black", linewidth = 0.3),
    strip.text = element_text(size = 7, face = "bold"),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6)
  )


# ══════════════════════════════════════════════════════════════════════════
# PANEL A: Study design
# ══════════════════════════════════════════════════════════════════════════

dxaimg <- readPNG(here("figures/archive/dxa.fig.png"))
biopsyimg <- readPNG(here("figures/archive/biopsy.fig.png"))
gtimg <- readPNG(here("figures/archive/blood.vial.png"))
mriimg <- readPNG(here("figures/archive/mri.png"))
strimg <- readPNG(here("figures/archive/str.png"))
rtimg <- readPNG(here("figures/archive/rt.fig.png"))
smallarrow <- readPNG(here("figures/archive/small.arrow.png"))
longarrow <- readPNG(here("figures/archive/long.arrow.png"))

d.dat <- read_excel(here("data/design.dat.xlsx"), na = "NA")

line_size <- 0.2
htextsize <- 2.9
ltextsize <- 2.3
textsize <- 2
stextsize <- 1.7

panel_A <- d.dat %>%
  ggplot(aes(time, tp)) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 24), expand = c(0, 0)) +
  
  geom_rect(
    aes(xmin = 0, xmax = 24, ymin = 0, ymax = 10),
    fill = "#f0f4f8", color = "#f0f4f8", linewidth = 0, inherit.aes = FALSE
  ) +
  annotate("segment", x = 0, xend = 24, y = 5, yend = 5, colour = "#7f8c8d", linewidth = 0.5) +
  annotate("segment", x = 18.5, xend = 18.5, y = 10, yend = 5, colour = "#7f8c8d", linewidth = 0.5) +
  
  annotate("segment", x = .2, xend = 23.8, y = .5, yend = .5) +
  annotate("segment", x = .2, xend = .2, y = .3, yend = .7) +
  annotate("segment", x = 3.5, xend = 3.5, y = .3, yend = .7) +
  annotate("segment", x = 10.9, xend = 10.9, y = .3, yend = .7) +
  annotate("segment", x = 13.1, xend = 13.1, y = .3, yend = .7) +
  annotate("segment", x = 20, xend = 20, y = .3, yend = .7) +
  annotate("segment", x = 23.8, xend = 23.8, y = .3, yend = .7) +
  
  annotate("text", x = 9.5, y = 9.7, label = "Recruitment", size = htextsize) +
  
  annotate("text", x = 1.4, y = 8.25, label = "Screening", size = ltextsize) +
  annotate("text", x = 1.4, y = 7.75, label = "Inc./Ex. criteria", size = ltextsize) +
  annotate("segment", x = 0.2, xend = 10, y = 7.25, yend = 7.25) +
  draw_image(smallarrow, x = 2.5, y = 6.77, scale = 0.5) +
  
  annotate("text", x = 5, y = 8.25, label = "Inclusion (n = 81)", size = ltextsize) +
  annotate("text", x = 5, y = 7.75, label = "Informed consent", size = ltextsize) +
  draw_image(smallarrow, x = 6.5, y = 6.77, scale = 0.5) +
  
  annotate("text", x = 8.5, y = 8.25, label = "Age", size = ltextsize) +
  annotate("text", x = 8.5, y = 7.75, label = "stratification", size = ltextsize) +
  annotate("segment", x = 10, xend = 11.5, y = 8.4, yend = 8.4) +
  annotate("segment", x = 10, xend = 10, y = 7.25, yend = 8.4) +
  draw_image(smallarrow, x = 11, y = 7.905, scale = 0.5) +
  annotate("segment", x = 10, xend = 11.5, y = 6.3, yend = 6.3) +
  annotate("segment", x = 10, xend = 10, y = 7.25, yend = 6.3) +
  draw_image(smallarrow, x = 11, y = 5.8, scale = 0.5) +
  
  annotate("text", x = 12.5, y = 8.4, label = "<30 yrs", size = ltextsize) +
  annotate("text", x = 12.5, y = 6.3, label = ">70 yrs", size = ltextsize) +
  
  annotate("text", x = 14.75, y = 7.35, label = "Randomization", size = ltextsize) +
  annotate("segment", x = 13.5, xend = 16, y = 8.4, yend = 8.4) +
  draw_image(smallarrow, x = 15.5, y = 7.905, scale = 0.5) +
  annotate("segment", x = 13.5, xend = 16, y = 6.3, yend = 6.3) +
  draw_image(smallarrow, x = 15.5, y = 5.8, scale = 0.5) +
  
  annotate("text", x = 16.75, y = 8.8, label = "LV", size = ltextsize) +
  annotate("text", x = 16.75, y = 8.1, label = "MV", size = ltextsize) +
  annotate("text", x = 17.4, y = 8.45, label = "n = 32", size = ltextsize) +
  annotate("text", x = 16.75, y = 7, label = "LV", size = ltextsize) +
  annotate("text", x = 16.75, y = 6.3, label = "MV", size = ltextsize) +
  annotate("text", x = 17.4, y = 6.65, label = "n = 39", size = ltextsize) +
  annotate("text", x = 16.75, y = 5.7, label = "CON", size = ltextsize) +
  annotate("text", x = 17.4, y = 5.3, label = "n = 10", size = ltextsize) +
  
  annotate("text", x = 20.5, y = 9.6, label = "DXA/US", size = textsize) +
  annotate("text", x = 20.5, y = 8.9, label = "Biopsy", size = textsize) +
  annotate("text", x = 20.5, y = 7.9, label = "OGGT", size = textsize) +
  annotate("text", x = 20.5, y = 7, label = "MRI", size = textsize) +
  annotate("text", x = 20.5, y = 6.2, label = "STR", size = textsize) +
  annotate("text", x = 20.5, y = 5.4, label = "RT", size = textsize) +
  draw_image(dxaimg, x = 21.5, y = 9.1, scale = 0.5) +
  draw_image(biopsyimg, x = 21.5, y = 8.26, scale = 0.6) +
  draw_image(gtimg, x = 21.5, y = 7.42, scale = 0.6) +
  draw_image(mriimg, x = 21.5, y = 6.58, scale = 0.6) +
  draw_image(strimg, x = 21.5, y = 5.74, scale = 0.6) +
  draw_image(rtimg, x = 21.5, y = 4.9, scale = 0.6) +
  
  annotate("text", x = 2, y = .2, label = "Week 0: Baseline", size = textsize) +
  annotate("text", x = .6, y = .7, label = "Pre 1", size = stextsize) +
  annotate("text", x = 1.75, y = .7, label = "Pre 2", size = stextsize) +
  annotate("text", x = 2.9, y = .7, label = "Pre 3", size = stextsize) +
  annotate("text", x = 3.9, y = .7, label = "Pre 4", size = stextsize) +
  annotate("text", x = 4.6, y = .2, label = "Week 1: Training", size = textsize) +
  draw_image(dxaimg, x = .1, y = 3.4, scale = .9) +
  draw_image(biopsyimg, x = .1, y = 2.2, scale = .9) +
  draw_image(gtimg, x = .1, y = 1, scale = .9) +
  draw_image(strimg, x = 1.2, y = 1, scale = .9) +
  draw_image(mriimg, x = 2.3, y = 1, scale = .9) +
  
  annotate("text", x = 12, y = 4.5, label = "Week 6: Mid", size = textsize) +
  annotate("text", x = 7.5, y = 3.2, label = "2x/week for 5 weeks", size = textsize) +
  annotate("text", x = 16.3, y = 3.2, label = "3x/week for 5 weeks", size = textsize) +
  annotate("text", x = 8, y = .2, label = "Week 3", size = textsize) +
  annotate("text", x = 12, y = .2, label = "Week 5-6: Mid", size = textsize) +
  annotate("text", x = 11.4, y = .7, label = "Mid 1/2", size = stextsize) +
  annotate("text", x = 12.5, y = .7, label = "Mid 3", size = stextsize) +
  draw_image(strimg, x = 3.4, y = 1, scale = .9) +
  draw_image(rtimg, x = 4.3, y = 3, scale = 1.2) +
  draw_image(longarrow, x = 7.7, y = 2.3, scale = 2, width = 8) +
  draw_image(biopsyimg, x = 7.5, y = .8, scale = 1) +
  draw_image(strimg, x = 10.9, y = 1, scale = 1) +
  draw_image(dxaimg, x = 11.9, y = 1, scale = 1) +
  draw_image(rtimg, x = 13.2, y = 3, scale = 1.2) +
  draw_image(strimg, x = 19.2, y = 1, scale = 1) +
  
  annotate("text", x = 22, y = .2, label = "Week 12: Post", size = textsize) +
  annotate("text", x = 20.8, y = .7, label = "Post 1", size = stextsize) +
  annotate("text", x = 22, y = .7, label = "Post 2", size = stextsize) +
  annotate("text", x = 23.2, y = .7, label = "Post 3", size = stextsize) +
  draw_image(dxaimg, x = 20.3, y = 2.2, scale = 1) +
  draw_image(biopsyimg, x = 20.3, y = 1, scale = 1) +
  draw_image(gtimg, x = 21.45, y = 1, scale = 1) +
  draw_image(mriimg, x = 22.6, y = 1, scale = 1) +
  
  theme_void()


# ══════════════════════════════════════════════════════════════════════════
# PANEL E: Weekly training volume
# ══════════════════════════════════════════════════════════════════════════

exercise <- reliefdata::relief_exercise |> 
  mutate(participant = as.character(participant))

participants <- reliefdata::relief_participants |> 
  mutate(age_group = if_else(age < 40, "yng", "old"))

exer.dat <- exercise |>
  full_join(participants, by = "participant")

# Checking participant count for design fig
# participants |> nrow()
# 81 in total

# How many per group/condition?
#condition <- reliefdata::relief_volume |> 
#  mutate(participant = as.character(participant))

#participants |>
#  full_join(condition, by = "participant") |>
#  mutate(condition = if_else(allocation == "con", "con", condition)) |>
#  distinct(participant, age_group, allocation, condition) |>
# count(age_group, allocation, condition)

# 49 old (39 int, 10 con), 32 young (int)

# Volume load per set -> session -> week
exer_session <- exer.dat |>
  mutate(vl = kg * rep) |>
  summarise(.by = c(participant, session, exercise, condition, leg, age_group, allocation),
            session_vl = sum(vl, na.rm = TRUE))

# Week mapping: weeks 1-5 at 2 sessions/week (sessions 1-10),
# weeks 6-10 at 3 sessions/week (sessions 11-24, week 10 partial with 2 sessions)
exer_week <- exer_session |>
  mutate(week = case_when(
    session <= 10 ~ ceiling(session / 2),
    session > 10  ~ 5 + ceiling((session - 10) / 3)
  )) |>
  summarise(.by = c(participant, week, exercise, condition, age_group, allocation),
            week_vl = sum(session_vl, na.rm = TRUE))

# Group-level summary: mean + 95% CI per week, age x condition, leg press only
exer_week_summary <- exer_week |>
  filter(exercise == "LP") |>
  summarise(.by = c(week, age_group, condition),
            m = mean(week_vl, na.rm = TRUE),
            se = sd(week_vl, na.rm = TRUE) / sqrt(n()),
            lwr = m - 1.96 * se,
            upr = m + 1.96 * se) |>
  mutate(age_group = factor(age_group, levels = c("yng", "old"), labels = c("Young", "Old")),
         condition = factor(condition, levels = c("low", "mod"), labels = c("Low", "Moderate")))

panel_E <- exer_week_summary |>
  ggplot(aes(week, m, color = age_group, linetype = condition, group = interaction(age_group, condition))) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = age_group), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  scale_color_manual(values = c(Young = col_yng, Old = col_old), name = "Age group") +
  scale_fill_manual(values = c(Young = col_yng, Old = col_old), guide = "none") +
  scale_linetype_manual(values = c(Low = "solid", Moderate = "dashed"), name = "Volume") +
  scale_x_continuous(breaks = 1:10) +
  labs(x = "Week", y = "Volume load (kg × reps)", title = "Weekly Training Volume") +
  base_theme +
  theme(legend.position = "right",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7))


# ══════════════════════════════════════════════════════════════════════════
# COMBINE
# ══════════════════════════════════════════════════════════════════════════

fig1 <- plot_grid(
  plot_grid(panel_A, labels = "A", label_size = 9),
  plot_grid(panel_E, labels = "B", label_size = 9),
  ncol = 1,
  rel_heights = c(1.6, 1)
)

ggsave(
  filename = "./figures/fig1-study-design.png",
  plot = fig1,
  device = "png",
  width = 174,
  height = 160,
  units = "mm",
  dpi = 300,
  bg = "white"
)

saveRDS(fig1, "./figures/fig1-study-design.RDS")



