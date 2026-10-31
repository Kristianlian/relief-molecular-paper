## Study design + exercise figure (molecular paper)
## CONSORT-style panel A; panel B = biopsy/RT schedule above weekly volume load

library(here)
library(tidyverse)
library(cowplot)
library(patchwork)

# ── Shared aesthetics ────────────────────────────────────────────────────────
col_yng <- "#2166ac"
col_old <- "#d6604d"
col_con <- "#4d4d4d"

base_theme <- theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.3),
    axis.ticks = element_line(colour = "black", linewidth = 0.3),
    strip.text = element_text(size = 9, face = "bold"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9)
  )

box_text_size <- 2.9
label_text_size <- 3.2

flow_arrow <- arrow(length = unit(0.1, "inches"), type = "closed")


# --- PANEL A: CONSORT-style flow diagram (same as the functional paper) ------

add_box <- function(p, x, y, w, h, label, size = box_text_size) {
  p +
    geom_rect(aes(xmin = x - w/2, xmax = x + w/2, ymin = y - h/2, ymax = y + h/2),
              fill = "white", color = "black", linewidth = 0.4, inherit.aes = FALSE) +
    annotate("text", x = x, y = y, label = label, size = size, lineheight = 0.85)
}

add_elbow <- function(p, x0, y0, x1, y1, with_arrow = TRUE) {
  ymid <- (y0 + y1) / 2
  p +
    geom_segment(aes(x = x0, xend = x0, y = y0, yend = ymid), linewidth = 0.4, inherit.aes = FALSE) +
    geom_segment(aes(x = x0, xend = x1, y = ymid, yend = ymid), linewidth = 0.4, inherit.aes = FALSE) +
    geom_segment(aes(x = x1, xend = x1, y = ymid, yend = y1),
                 linewidth = 0.4, inherit.aes = FALSE,
                 arrow = if (with_arrow) flow_arrow else NULL)
}

panel_A1 <- ggplot() +
  scale_x_continuous(limits = c(0, 12), expand = c(0, 0)) +
  scale_y_continuous(limits = c(2.6, 10), expand = c(0, 0)) +
  theme_void()

# Enrollment
panel_A1 <- add_box(panel_A1, x = 5.75, y = 9.3, w = 6.5, h = 1.1,
                    label = "Screened & enrolled\n(n = 81)")

# Enrollment -> age groups
panel_A1 <- add_elbow(panel_A1, x0 = 5.75, y0 = 8.75, x1 = 3,   y1 = 8.05)
panel_A1 <- add_elbow(panel_A1, x0 = 5.75, y0 = 8.75, x1 = 8.5, y1 = 8.05)

# Age groups
panel_A1 <- add_box(panel_A1, x = 3,   y = 7.5, w = 4.5, h = 1.1,
                    label = "Younger adults\n(< 30 years, n = 32)")
panel_A1 <- add_box(panel_A1, x = 8.5, y = 7.5, w = 4.5, h = 1.1,
                    label = "Older adults\n(> 70 years, n = 49)")

# Randomization labels
panel_A1 <- panel_A1 +
  annotate("text", x = 4.8, y = 5.8, label = "Randomized to training volume",
           size = box_text_size - 0.3, fontface = "italic") +
  annotate("text", x = 9.8, y = 6.3, label = "Randomized to training/control",
           size = box_text_size - 0.3, fontface = "italic")

# Age groups -> allocation
panel_A1 <- add_elbow(panel_A1, x0 = 3,   y0 = 6.95, x1 = 3,    y1 = 4.75)
panel_A1 <- add_elbow(panel_A1, x0 = 8.5, y0 = 6.95, x1 = 6.6,  y1 = 4.75)
panel_A1 <- add_elbow(panel_A1, x0 = 8.5, y0 = 6.95, x1 = 10.4, y1 = 4.75)

# Allocation boxes
panel_A1 <- add_box(panel_A1, x = 3,    y = 4.2, w = 4.5, h = 1.3,
                    label = "Low volume (LV) +\nModerate volume (MV)\n(n = 32)")
panel_A1 <- add_box(panel_A1, x = 6.6,  y = 4.2, w = 4,   h = 1.3,
                    label = "Low volume (LV) +\nModerate volume (MV)\n(n = 39)")
panel_A1 <- add_box(panel_A1, x = 10.4, y = 4.2, w = 2.3, h = 1.3,
                    label = "Control (CON)\n(n = 10)")


# --- PANEL B1: Biopsy schedule + RT session ticks, aligned to the volume plot -

# Axis: week 1 = baseline, training = weeks 2-11, post = week 12.
# The week-3 biopsy is taken after six sessions (end of training week 3 = axis week 4).
test_order <- c("Biopsy", "RT")

biopsy_points <- tibble(weeks = c(1, 4, 12),
                        label = c("Baseline", "Week 3", "Post"),
                        hj    = c(0.15, 0.5, 0.5),    # left-align "Baseline" so it stays inside the panel
                        test  = factor("Biopsy", levels = test_order))

guide_weeks <- c(1, 4, 12)
week_breaks <- 1:12

# RT session tick positions
exercise <- reliefdata::relief_exercise |>
  mutate(participant = as.character(participant))

participants <- reliefdata::relief_participants |>
  mutate(age_group = if_else(age < 40, "yng", "old"))

exer.dat <- exercise |>
  full_join(participants, by = "participant")

exer_session <- exer.dat |>
  mutate(vl = kg * rep) |>
  summarise(.by = c(participant, session, exercise, condition, leg, age_group, allocation),
            session_vl = sum(vl, na.rm = TRUE))

session_weeks <- exer_session |>
  distinct(session) |>
  mutate(week = case_when(
    session <= 10 ~ ceiling(session / 2),
    session > 10  ~ 5 + ceiling((session - 10) / 3)
  )) |>
  mutate(week = week + 1) |>
  arrange(week, session) |>
  mutate(.by = week,
         session_in_week = row_number(),
         n_in_week = n()) |>
  mutate(x_pos = week + (session_in_week - 0.5) / n_in_week * 0.8,
         test = factor("RT", levels = test_order))

p_schedule <- ggplot() +
  geom_vline(xintercept = guide_weeks, linetype = 2, color = "grey70", linewidth = 0.4) +
  geom_point(data = biopsy_points, aes(x = weeks, y = test), size = 3, shape = 16) +
  geom_text(data = biopsy_points, aes(x = weeks, y = test, label = label, hjust = hj),
            vjust = -1.3, size = label_text_size) +
  geom_point(data = session_weeks, aes(x = x_pos, y = test),
             shape = 108, size = 3.5, color = "grey30") +
  scale_y_discrete(limits = rev(test_order), expand = expansion(add = c(0.6, 0.9))) +
  scale_x_continuous(limits = c(0.8, 12.7), breaks = week_breaks,
                     labels = as.character(week_breaks), expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL) +
  base_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        plot.margin = margin(t = 5, r = 5, b = 2, l = 5))


# --- PANEL B2: Weekly training volume load (all exercises combined) ----------

exer_week <- exer_session |>
  mutate(week = case_when(
    session <= 10 ~ ceiling(session / 2),
    session > 10  ~ 5 + ceiling((session - 10) / 3)
  )) |>
  mutate(week = week + 1) |>
  summarise(.by = c(participant, week, condition, age_group, allocation),
            week_vl = sum(session_vl, na.rm = TRUE))   # all exercises; as in the functional paper

exer_week_summary <- exer_week |>
  summarise(.by = c(week, age_group, condition),
            m = mean(week_vl, na.rm = TRUE),
            se = sd(week_vl, na.rm = TRUE) / sqrt(n()),
            lwr = m - 1.96 * se,
            upr = m + 1.96 * se) |>
  mutate(age_group = factor(age_group, levels = c("yng", "old"), labels = c("Young", "Old")),
         condition = factor(condition, levels = c("low", "mod"), labels = c("Low", "Moderate")))

p_volume <- exer_week_summary |>
  ggplot(aes(week, m, color = age_group, linetype = condition,
             group = interaction(age_group, condition))) +
  geom_vline(xintercept = guide_weeks, linetype = 2, color = "grey70", linewidth = 0.4) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = age_group), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  scale_color_manual(values = c(Young = col_yng, Old = col_old), name = "Age group") +
  scale_fill_manual(values = c(Young = col_yng, Old = col_old), guide = "none") +
  scale_linetype_manual(values = c(Low = "solid", Moderate = "dashed"), name = "Volume") +
  scale_x_continuous(limits = c(0.8, 12.7), breaks = week_breaks,
                     labels = as.character(week_breaks), expand = c(0.01, 0.01)) +
  labs(x = "Week", y = "Weekly volume load (kg \u00d7 reps)") +
  base_theme +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        plot.margin = margin(t = 2, r = 5, b = 5, l = 5))


# --- COMBINE B: schedule (biopsies + RT ticks) / weekly volume, shared x-axis -

panel_B <- p_schedule / p_volume + plot_layout(heights = c(0.9, 2.2))


# --- COMBINE A + B -----------------------------------------------------------

fig_study_design <- plot_grid(
  plot_grid(panel_A1, labels = "A", label_size = 12),
  plot_grid(panel_B, labels = "B", label_size = 12),
  ncol = 1,
  rel_heights = c(1, 1.1)
)

ggsave(
  filename = "./figures/fig-study-design.png",
  plot = fig_study_design,
  device = "png",
  width = 180,
  height = 190,
  units = "mm",
  dpi = 300,
  bg = "white"
)

saveRDS(fig_study_design, "./figures/fig-study-design.RDS")
