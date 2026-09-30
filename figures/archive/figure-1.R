#' ---
#' title: Figure 1 (design, muscle volume, HUMAC isometric/60, training load)
#' format: html
#' editor_options:
#'   chunk_output_type: console
#' ---

# Comments
# Looks quite clean, but a couple things to do:
# Decide if you want to split by sex in both, or stick to volume condition groups for both,
# sort out legends when the above is decided. Currently muscle volume has male a blue and
# female as red

library(here)
library(tidyverse)
library(cowplot)
library(readxl)   
library(png)      

# ── Shared aesthetics ────────────────────────────────────────────────────────
col_yng <- "#2166ac"
col_old <- "#d6604d"
col_con <- "#4d4d4d"   # control, kept distinct from Old
lty_low <- "solid"
lty_mod <- "dashed"

base_theme <- theme_minimal(base_size = 8) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(colour = "black", linewidth = 0.3),
    axis.ticks = element_line(colour = "black", linewidth = 0.3),
    legend.position = "none",
    strip.text = element_text(size = 7, face = "bold"),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 6)
  )

facet_order <- c(
  "Old, Low volume", "Old, Mod volume",
  "Young, Low volume", "Young, Mod volume",
  "Old, Control"
)

parse_tx <- function(tx) {
  case_when(
    tx == "int_old_low" ~ "Old, Low volume",
    tx == "int_old_mod" ~ "Old, Mod volume",
    tx == "int_yng_low" ~ "Young, Low volume",
    tx == "int_yng_mod" ~ "Young, Mod volume",
    tx == "con_old_con" ~ "Old, Control"
  )
}

add_facet_grp <- function(df, tx_col = "tx") {
  df %>%
    mutate(
      facet_grp = factor(parse_tx(.data[[tx_col]]), levels = facet_order),
      age_grp = if_else(str_detect(facet_grp, "^Old"), "Old", "Young"),
      age_grp = factor(age_grp, levels = c("Young", "Old")),
      condition = case_when(
        str_detect(facet_grp, "Low")     ~ "Low",
        str_detect(facet_grp, "Mod")     ~ "Moderate",
        str_detect(facet_grp, "Control") ~ "Control"
      ),
      condition = factor(condition, levels = c("Low", "Moderate", "Control"))
    )
}

# ── Read data ────────────────────────────────────────────────────────────────
pred_mv  <- readRDS(here("data/data-gen/pred_mv.rds"))
contr_mv <- readRDS(here("data/data-gen/contrast_mv.rds"))

pred_humac_isom  <- readRDS(here("data/data-gen/pred_humac_isom.rds"))
contr_humac_isom <- readRDS(here("data/data-gen/contrast_humac_isom.rds"))

pred_humac_60  <- readRDS(here("data/data-gen/pred_humac_60.rds"))
contr_humac_60 <- readRDS(here("data/data-gen/contrast_humac_60.rds"))

emm_vl <- readRDS(here("data/data-gen/emm_log_df.rds")) %>%
  as.data.frame() %>%
  mutate(
    age_grp = factor(age_grp, levels = c("yng", "old"), labels = c("Young", "Old")),
    condition = factor(condition, levels = c("low", "mod"), labels = c("Low", "Moderate"))
  )

# ── Panel A: Study design ────────────────────────────────────────────────────
# Images
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
  
  # Background
  geom_rect(
    aes(xmin = 0, xmax = 24, ymin = 0, ymax = 10),
    fill = "#f0f4f8",
    color = "#f0f4f8",
    linewidth = 0,
    inherit.aes = FALSE
  ) +
  annotate(
    "segment",
    x = 0,
    xend = 24,
    y = 5,
    yend = 5,
    colour = "#7f8c8d",
    linewidth = 0.5
  ) +
  annotate(
    "segment",
    x = 18.5,
    xend = 18.5,
    y = 10,
    yend = 5,
    colour = "#7f8c8d",
    linewidth = 0.5
  ) +
  
  # Timeline
  annotate("segment", x = .2, xend = 23.8, y = .5, yend = .5) +
  annotate("segment", x = .2, xend = .2, y = .3, yend = .7) +
  annotate("segment", x = 3.5, xend = 3.5, y = .3, yend = .7) +
  annotate("segment", x = 10.9, xend = 10.9, y = .3, yend = .7) +
  annotate("segment", x = 13.1, xend = 13.1, y = .3, yend = .7) +
  annotate("segment", x = 20, xend = 20, y = .3, yend = .7) +
  annotate("segment", x = 23.8, xend = 23.8, y = .3, yend = .7) +
  
  # Recruitment heading
  annotate("text", x = 9.5, y = 9.7, label = "Recruitment", size = htextsize) +
  
  # Screening
  annotate("text", x = 1.4, y = 8.25, label = "Screening", size = ltextsize) +
  annotate(
    "text",
    x = 1.4,
    y = 7.75,
    label = "Inc./Ex. criteria",
    size = ltextsize
  ) +
  annotate("segment", x = 0.2, xend = 10, y = 7.25, yend = 7.25) +
  draw_image(smallarrow, x = 2.5, y = 6.77, scale = 0.5) +
  
  # Inclusion
  annotate(
    "text",
    x = 5,
    y = 8.25,
    label = "Inclusion (n = )",
    size = ltextsize
  ) +
  annotate(
    "text",
    x = 5,
    y = 7.75,
    label = "Informed consent",
    size = ltextsize
  ) +
  draw_image(smallarrow, x = 6.5, y = 6.77, scale = 0.5) +
  
  # Age stratification
  annotate("text", x = 8.5, y = 8.25, label = "Age", size = ltextsize) +
  annotate(
    "text",
    x = 8.5,
    y = 7.75,
    label = "stratification",
    size = ltextsize
  ) +
  annotate("segment", x = 10, xend = 11.5, y = 8.4, yend = 8.4) +
  annotate("segment", x = 10, xend = 10, y = 7.25, yend = 8.4) +
  draw_image(smallarrow, x = 11, y = 7.905, scale = 0.5) +
  annotate("segment", x = 10, xend = 11.5, y = 6.3, yend = 6.3) +
  annotate("segment", x = 10, xend = 10, y = 7.25, yend = 6.3) +
  draw_image(smallarrow, x = 11, y = 5.8, scale = 0.5) +
  
  # Groups
  annotate("text", x = 12.5, y = 8.4, label = "<30 yrs", size = ltextsize) +
  annotate("text", x = 12.5, y = 6.3, label = ">70 yrs", size = ltextsize) +
  
  # Randomization
  annotate(
    "text",
    x = 14.75,
    y = 7.35,
    label = "Randomization",
    size = ltextsize
  ) +
  annotate("segment", x = 13.5, xend = 16, y = 8.4, yend = 8.4) +
  draw_image(smallarrow, x = 15.5, y = 7.905, scale = 0.5) +
  annotate("segment", x = 13.5, xend = 16, y = 6.3, yend = 6.3) +
  draw_image(smallarrow, x = 15.5, y = 5.8, scale = 0.5) +
  
  # Volumes
  annotate("text", x = 16.75, y = 8.8, label = "LV", size = ltextsize) +
  annotate("text", x = 16.75, y = 8.1, label = "MV", size = ltextsize) +
  annotate("text", x = 16.75, y = 7, label = "LV", size = ltextsize) +
  annotate("text", x = 16.75, y = 6.3, label = "MV", size = ltextsize) +
  annotate("text", x = 16.75, y = 5.7, label = "CON", size = ltextsize) +
  
  # Symbol legend (right panel)
  annotate("text", x = 20.5, y = 9.6, label = "DXA/US", size = textsize) +
  annotate("text", x = 20.5, y = 8.9, label = "Biopsy", size = textsize) +
  annotate("text", x = 20.5, y = 7.9, label = "GT", size = textsize) +
  annotate("text", x = 20.5, y = 7, label = "MRI", size = textsize) +
  annotate("text", x = 20.5, y = 6.2, label = "STR", size = textsize) +
  annotate("text", x = 20.5, y = 5.4, label = "RT", size = textsize) +
  draw_image(dxaimg, x = 21.5, y = 9.1, scale = 0.5) +
  draw_image(biopsyimg, x = 21.5, y = 8.26, scale = 0.6) +
  draw_image(gtimg, x = 21.5, y = 7.42, scale = 0.6) +
  draw_image(mriimg, x = 21.5, y = 6.58, scale = 0.6) +
  draw_image(strimg, x = 21.5, y = 5.74, scale = 0.6) +
  draw_image(rtimg, x = 21.5, y = 4.9, scale = 0.6) +
  
  # Baseline block
  annotate("text", x = 2, y = .2, label = "Week 1: Baseline", size = textsize) +
  annotate("text", x = .6, y = .7, label = "Pre 1", size = stextsize) +
  annotate("text", x = 1.75, y = .7, label = "Pre 2", size = stextsize) +
  annotate("text", x = 2.9, y = .7, label = "Pre 3", size = stextsize) +
  annotate("text", x = 3.9, y = .7, label = "Pre 4", size = stextsize) +
  draw_image(dxaimg, x = .1, y = 3.4, scale = .9) +
  draw_image(biopsyimg, x = .1, y = 2.2, scale = .9) +
  draw_image(gtimg, x = .1, y = 1, scale = .9) +
  draw_image(strimg, x = 1.2, y = 1, scale = .9) +
  draw_image(mriimg, x = 2.3, y = 1, scale = .9) +
  
  # Intervention block
  annotate("text", x = 12, y = 4.5, label = "Week 6: Mid", size = textsize) +
  annotate(
    "text",
    x = 7.5,
    y = 3.2,
    label = "2x/week for 5 weeks",
    size = textsize
  ) +
  annotate(
    "text",
    x = 16.3,
    y = 3.2,
    label = "3x/week for 5 weeks",
    size = textsize
  ) +
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
  
  # Post block
  annotate("text", x = 22, y = .2, label = "Week 12: Post", size = textsize) +
  annotate("text", x = 20.8, y = .7, label = "Post 1", size = stextsize) +
  annotate("text", x = 22, y = .7, label = "Post 2", size = stextsize) +
  annotate("text", x = 23.2, y = .7, label = "Post 3", size = stextsize) +
  draw_image(dxaimg, x = 20.3, y = 2.2, scale = 1) +
  draw_image(biopsyimg, x = 20.3, y = 1, scale = 1) +
  draw_image(gtimg, x = 21.45, y = 1, scale = 1) +
  draw_image(mriimg, x = 22.6, y = 1, scale = 1) +
  
  theme_void()

# ── Muscle volume: predictions only, facet labels annotated with credibility ─
mv_delta <- contr_mv %>%
  filter(hypothesis %in% c(
    "int_old_low_delta", "int_old_mod_delta",
    "int_yng_low_delta", "int_yng_mod_delta", "con_old_con_delta"
  )) %>%
  mutate(
    facet_grp = case_when(
      hypothesis == "int_old_low_delta" ~ "Old, Low volume",
      hypothesis == "int_old_mod_delta" ~ "Old, Mod volume",
      hypothesis == "int_yng_low_delta" ~ "Young, Low volume",
      hypothesis == "int_yng_mod_delta" ~ "Young, Mod volume",
      hypothesis == "con_old_con_delta" ~ "Old, Control"
    ),
    credible = lower95 > 1 | upper95 < 1,
    facet_label = if_else(credible, paste0(facet_grp, " **"), facet_grp)   # was " *"
  )

# Named vector: tx code -> annotated facet label, in facet_order sequence
mv_labels <- setNames(mv_delta$facet_label[match(facet_order, mv_delta$facet_grp)],
                      c("int_old_low", "int_old_mod", "int_yng_low", "int_yng_mod", "con_old_con"))

p_mv_pred <- ggplot(pred_mv, aes(time, m, colour = sex, group = paste(tx, sex))) +
  geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0.1, linewidth = 0.35, show.legend = FALSE) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.5) +
  facet_wrap(~tx, nrow = 1, labeller = labeller(tx = mv_labels)) +
  scale_colour_discrete(name = "Sex", labels = c("Female", "Male")) +
  labs(x = NULL, y = "Muscle Volume (cm3)") +
  base_theme +
  theme(plot.caption = element_text(size = 6, hjust = 0))

# ── HUMAC: same pattern, additive scale (null = 0) ──────────────────────────
make_humac_pred_plot <- function(dat, contr_dat, ylab) {
  contr_dat <- contr_dat %>%
    add_facet_grp("tx") %>%
    mutate(
      credible = lower95 > 0 | upper95 < 0,
      facet_label = if_else(credible, paste0(facet_grp, " **"), as.character(facet_grp))
    )
  
  humac_labels <- setNames(
    contr_dat$facet_label[match(facet_order, contr_dat$facet_grp)],
    c("int_old_low", "int_old_mod", "int_yng_low", "int_yng_mod", "con_old_con")
  )
  
  dat <- dat %>%
    add_facet_grp("tx") %>%
    mutate(time = factor(time, levels = c("pre", "post"), labels = c("Pre", "Post")))
  
  ggplot(dat, aes(time, m, colour = age_grp, linetype = condition, group = tx)) +
    geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0.1, linewidth = 0.35, show.legend = FALSE) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    facet_wrap(~tx, nrow = 1, labeller = labeller(tx = humac_labels)) +
    scale_colour_manual(values = c(Young = col_yng, Old = col_old), name = "Age group") +
    scale_linetype_manual(values = c(Low = lty_low, Moderate = lty_mod, Control = "dotted"), name = "Volume") +
    labs(x = NULL, y = ylab) +
    base_theme 
}

p_isom_pred <- make_humac_pred_plot(pred_humac_isom, contr_humac_isom, "Peak Torque (Nm)\nIsometric")
p_60_pred   <- make_humac_pred_plot(pred_humac_60,   contr_humac_60,   "Peak Torque (Nm)\n60°/s")

# ── Training load (descriptive, no contrast) ────────────────────────────────
make_vl_plot <- function(dat, ylab = "Volume Load (kg x reps)", title = "Exercise Volume") {
  ggplot(dat, aes(x = condition, y = emmean, fill = age_grp, colour = age_grp)) +
    geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.7, linewidth = 0.3) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(0.7), width = 0.25, linewidth = 0.35) +
    scale_fill_manual(values = c(Young = col_yng, Old = col_old)) +
    scale_colour_manual(values = c(Young = col_yng, Old = col_old)) +
    labs(title = title, x = "Volume condition", y = ylab) +
    base_theme
}

panel_E <- make_vl_plot(emm_vl)

# ── Panels B/C/D become single-plot rows, not pred|contr pairs ──────────────
panel_B <- p_mv_pred
panel_C <- p_isom_pred
panel_D <- p_60_pred

# panel_A assumed already built by the design-figure script — source it or
# paste that chunk here unmodified (images, d.dat, draw_image calls, theme_void())

fig1 <- plot_grid(
  plot_grid(panel_A, labels = "A", label_size = 9),
  plot_grid(panel_B, labels = "B", label_size = 9),
  plot_grid(panel_C, labels = "C", label_size = 9),
  plot_grid(panel_D, labels = "D", label_size = 9),
  plot_grid(panel_E, labels = "E", label_size = 9),
  ncol = 1,
  rel_heights = c(1.6, 1, 1, 1, 1)
)

ggsave(
  filename = here("figures/figure-1.png"),
  plot = fig1,
  device = "png",
  width = 174,
  height = 280,
  units = "mm",
  dpi = 300,
  bg = "white"
)

saveRDS(fig1, ("./figures/figure-1.RDS"))
