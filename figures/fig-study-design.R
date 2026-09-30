
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

line_size <- 0.2
htextsize <- 2.9
ltextsize <- 2.3
textsize <- 2
stextsize <- 1.7


# --- LOAD IMAGES + DATA ------------------------------------------------------

dxaimg <- readPNG(here("figures/archive/dxa.fig.png"))
biopsyimg <- readPNG(here("figures/archive/biopsy.fig.png"))
gtimg <- readPNG(here("figures/archive/blood.vial.png"))
mriimg <- readPNG(here("figures/archive/mri.png"))
strimg <- readPNG(here("figures/archive/str.png"))
rtimg <- readPNG(here("figures/archive/rt.fig.png"))

d.dat <- read_excel(here("data/design.dat.xlsx"), na = "NA")


# --- PANEL A1: Recruitment flowchart (no time axis, tightened y-range) -------

flow_arrow <- arrow(length = unit(0.09, "inches"), type = "closed")

panel_A1 <- ggplot() +
  scale_y_continuous(limits = c(4.5, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 19), expand = c(0, 0)) +
  
#  geom_rect(
#    aes(xmin = 0, xmax = 19, ymin = 4.5, ymax = 10),
#    fill = "#f0f4f8", color = "#f0f4f8", linewidth = 0, inherit.aes = FALSE
#  ) +
  
  annotate("text", x = 9.5, y = 9.7, label = "Recruitment", size = htextsize) +
  
  annotate("text", x = 1.4, y = 8.25, label = "Screening", size = ltextsize) +
  annotate("text", x = 1.4, y = 7.75, label = "Inc./Ex. criteria", size = ltextsize) +
  
  # Screening -> Inclusion -> Age stratification, arrowheads mark each step
  annotate("segment", x = 0.2, xend = 2.5, y = 7.25, yend = 7.25, arrow = flow_arrow) +
  annotate("segment", x = 2.5, xend = 6.5, y = 7.25, yend = 7.25, arrow = flow_arrow) +
  annotate("segment", x = 6.5, xend = 10, y = 7.25, yend = 7.25) +
  
  annotate("text", x = 5, y = 8.25, label = "Inclusion (n = 81)", size = ltextsize) +
  annotate("text", x = 5, y = 7.75, label = "Informed consent", size = ltextsize) +
  
  annotate("text", x = 8.5, y = 8.25, label = "Age", size = ltextsize) +
  annotate("text", x = 8.5, y = 7.75, label = "stratification", size = ltextsize) +
  
  # Age stratification branch -> <30 yrs
  annotate("segment", x = 10, xend = 10, y = 7.25, yend = 8.4) +
  annotate("segment", x = 10, xend = 11.5, y = 8.4, yend = 8.4, arrow = flow_arrow) +
  
  # Age stratification branch -> >70 yrs
  annotate("segment", x = 10, xend = 10, y = 7.25, yend = 6.3) +
  annotate("segment", x = 10, xend = 11.5, y = 6.3, yend = 6.3, arrow = flow_arrow) +
  
  annotate("text", x = 12.5, y = 8.4, label = "<30 yrs", size = ltextsize) +
  annotate("text", x = 12.5, y = 6.3, label = ">70 yrs", size = ltextsize) +
  
  annotate("text", x = 14.75, y = 7.35, label = "Randomization", size = ltextsize) +
  
  # Randomization -> LV/MV group (young)
  annotate("segment", x = 13.5, xend = 16, y = 8.4, yend = 8.4, arrow = flow_arrow) +
  
  # Randomization -> LV/MV/CON group (old)
  annotate("segment", x = 13.5, xend = 16, y = 6.3, yend = 6.3, arrow = flow_arrow) +
  
  annotate("text", x = 16.75, y = 8.8, label = "LV", size = ltextsize) +
  annotate("text", x = 16.75, y = 8.1, label = "MV", size = ltextsize) +
  annotate("text", x = 17.4, y = 8.45, label = "n = 32", size = ltextsize) +
  annotate("text", x = 16.75, y = 7, label = "LV", size = ltextsize) +
  annotate("text", x = 16.75, y = 6.3, label = "MV", size = ltextsize) +
  annotate("text", x = 17.4, y = 6.65, label = "n = 39", size = ltextsize) +
  annotate("text", x = 16.75, y = 5.7, label = "CON", size = ltextsize) +
  annotate("text", x = 17.4, y = 5.3, label = "n = 10", size = ltextsize) +
  
  theme_void()


# --- LEGEND STRIP: icon key, sits between Panel A and Panel B ----------------

panel_legend <- ggplot() +
  scale_y_continuous(limits = c(0, 2.2), expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, 7), expand = c(0, 0)) +
  
  draw_image(dxaimg,    x = 0.7, y = 1.3, scale = .5, hjust = 0.5, vjust = 0.5) +
  annotate("text", x = 0.7, y = 0.75, label = "DXA/US", size = textsize) +
  
  draw_image(biopsyimg, x = 1.9, y = 1.3, scale = .5, hjust = 0.5, vjust = 0.5) +
  annotate("text", x = 1.9, y = 0.75, label = "Biopsy", size = textsize) +
  
  draw_image(gtimg,     x = 3.1, y = 1.3, scale = .5, hjust = 0.5, vjust = 0.5) +
  annotate("text", x = 3.1, y = 0.75, label = "OGGT", size = textsize) +
  
  draw_image(mriimg,    x = 4.3, y = 1.3, scale = .5, hjust = 0.5, vjust = 0.5) +
  annotate("text", x = 4.3, y = 0.75, label = "MRI", size = textsize) +
  
  draw_image(strimg,    x = 5.5, y = 1.3, scale = .5, hjust = 0.5, vjust = 0.5) +
  annotate("text", x = 5.5, y = 0.75, label = "STR", size = textsize) +
  
  draw_image(rtimg, x = 6.5, y = 1.3, scale = .45, hjust = 0.5, vjust = 0.5) +
  annotate("text", x = 6.5, y = 0.75, label = "RT", size = textsize) +
  
  theme_void()


# --- PANEL B: Training volume + assessment markers, single shared axis -------

exercise <- reliefdata::relief_exercise |>
  mutate(participant = as.character(participant))

participants <- reliefdata::relief_participants |>
  mutate(age_group = if_else(age < 40, "yng", "old"))

exer.dat <- exercise |>
  full_join(participants, by = "participant")

# Volume load per set -> session -> week
exer_session <- exer.dat |>
  mutate(vl = kg * rep) |>
  summarise(.by = c(participant, session, exercise, condition, leg, age_group, allocation),
            session_vl = sum(vl, na.rm = TRUE))

# Week mapping: weeks 2-6 at 2 sessions/week (sessions 1-10),
# weeks 6-11 at 3 sessions/week (sessions 11-24, week 10 partial with 2 sessions)
exer_week <- exer_session |>
  mutate(week = case_when(
    session <= 10 ~ ceiling(session / 2),
    session > 10  ~ 5 + ceiling((session - 10) / 3)
  )) |>
  mutate(week = week + 1) |>   # shift so baseline = week 1, training = weeks 2-11
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

guide_weeks <- c(1, 6.5, 12)   # baseline, mid, post

label_y <- 23500   # shared top-anchor for every timepoint label

panel_B <- exer_week_summary |>
  ggplot(aes(week, m, color = age_group, linetype = condition, group = interaction(age_group, condition))) +
  
  geom_vline(xintercept = guide_weeks, linetype = 2, color = "grey70", linewidth = 0.4) +
  
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = age_group), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.8) +
  
  # Baseline & training start
  annotate("text", x = 1, y = label_y, label = "Baseline",
           size = textsize, vjust = 1) +
  draw_image(dxaimg,    x = 1, y = 21800, scale = 1400, hjust = 0.5, vjust = 0.5) +
  draw_image(biopsyimg, x = 1, y = 19800, scale = 1400, hjust = 0.5, vjust = 0.5) +
  draw_image(gtimg,     x = 1, y = 17800, scale = 1400, hjust = 0.5, vjust = 0.5) +
  draw_image(mriimg,    x = 1, y = 15800, scale = 1400, hjust = 0.5, vjust = 0.5) +
  draw_image(strimg,    x = 1.5, y = 13800, scale = 1400, hjust = 0.5, vjust = 0.5) + 
  
  # RT — RT start marker
  annotate("text", x = 2, y = label_y, label = "RT starts",
           size = textsize, lineheight = 0.8, vjust = 1) +
  draw_image(rtimg, x = 2, y = 21800, scale = 1700, hjust = 0.5, vjust = 0.5) +
  
  # Mid-testing
  annotate("text", x = 6.5, y = label_y, label = "Half-way",
           size = textsize, vjust = 1) +
  draw_image(strimg, x = 6.1, y = 21800, scale = 1400, hjust = 0.5, vjust = 0.5) +
  draw_image(dxaimg, x = 6.9, y = 21800, scale = 1400, hjust = 0.5, vjust = 0.5) +
  
  # Week 3 biopsy
  annotate("text", x = 4, y = label_y, label = "Biopsy",
           size = textsize, lineheight = 0.8, vjust = 1) +
  draw_image(biopsyimg, x = 4, y = 21800, scale = 1400, hjust = 0.5, vjust = 0.5) +
  
  # Post-testing
  annotate("text", x = 12, y = label_y, label = "Post",
           size = textsize, vjust = 1) +
  draw_image(dxaimg,    x = 12, y = 21800, scale = 1400, hjust = 0.5, vjust = 0.5) +
  draw_image(biopsyimg, x = 12, y = 19800, scale = 1400, hjust = 0.5, vjust = 0.5) +
  draw_image(gtimg,     x = 12, y = 17800, scale = 1400, hjust = 0.5, vjust = 0.5) +
  draw_image(mriimg,    x = 12, y = 15800, scale = 1400, hjust = 0.5, vjust = 0.5) +
  draw_image(strimg,    x = 11.5, y = 13800, scale = 1400, hjust = 0.5, vjust = 0.5) + 
  
  scale_color_manual(values = c(Young = col_yng, Old = col_old), name = "Age group") +
  scale_fill_manual(values = c(Young = col_yng, Old = col_old), guide = "none") +
  scale_linetype_manual(values = c(Low = "solid", Moderate = "dashed"), name = "Volume") +
  scale_x_continuous(limits = c(0.8, 12.7), breaks = c(1, 2, 4, 6.5, 11, 12),
                     labels = c("1", "2", "4", "6.5", "11", "12"), expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 25500)) +
  coord_cartesian(clip = "off") +
  labs(x = "Week", y = "Volume load (kg x reps)") +   # title removed
  base_theme +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        plot.margin = margin(t = 45, r = 5, b = 5, l = 5))


# --- COMBINE -----------------------------------------------------------------

fig_study_design <- plot_grid(
  plot_grid(panel_A1, labels = "A", label_size = 9),
  panel_legend,
  plot_grid(panel_B, labels = "B", label_size = 9),
  ncol = 1,
  rel_heights = c(1.1, 0.15, 1.3)
)

ggsave(
  filename = "./figures/fig-study-design.png",
  plot = fig_study_design,
  device = "png",
  width = 174,
  height = 190,
  units = "mm",
  dpi = 300,
  bg = "white"
)

saveRDS(fig_study_design, "./figures/fig-study-design.RDS")
