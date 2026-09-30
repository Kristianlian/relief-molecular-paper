#' ---
#' title: Figure 1
#' format: html
#' editor_options:
#'   chunk_output_type: console
#' ---
#' 



library(here)
knitr::opts_knit$set(root.dir = here::here())
library(tidyverse)
library(cowplot)
library(grid)
library(gridExtra)
library(ggtext)
library(readxl)
library(png)
library(magick)

# ── Shared aesthetics ────────────────────────────────────────────────────────
col_yng  <- "#2166ac"   # blue  – young
col_old  <- "#d6604d"   # red   – old
lty_low  <- "solid"
lty_mod  <- "dashed"

base_theme <- theme_minimal(base_size = 8) +
  theme(
    panel.grid       = element_blank(),
    axis.line        = element_line(colour = "black", linewidth = 0.3),
    axis.ticks       = element_line(colour = "black", linewidth = 0.3),
    legend.position  = "none",
    strip.text       = element_text(size = 7, face = "bold"),
    plot.title       = element_text(size = 8, face = "bold", hjust = 0.5),
    axis.title       = element_text(size = 7),
    axis.text        = element_text(size = 6)
  )

# Time factor order (shared)
time_levels <- c("pre", "mid", "post")
time_labels <- c("Pre", "Mid", "Post")


# Body composition
emm_mv  <- readRDS(here("data/data-gen/emm_mv.rds"))
emm_mt  <- readRDS(here("data/data-gen/emm_mt.rds"))
emm_ublm <- readRDS(here("data/data-gen/emm_ublm.rds"))

# Strength
emm_isom   <- readRDS(here("data/data-gen/emm_isom.rds"))
emm_isok60 <- readRDS(here("data/data-gen/emm_isok60.rds"))
emm_isok120<- readRDS(here("data/data-gen/emm_isok120.rds"))
emm_isok240<- readRDS(here("data/data-gen/emm_isok240.rds"))
emm_bc     <- readRDS(here("data/data-gen/emm_bc.rds"))

# Exercise (relative volume load)
emm_vl <- readRDS(here("data/data-gen/emm_log_df.rds"))


# Helper: standardise column names and factor levels
prep_emmeans <- function(df, outcome_label) {
  df %>%
    as.data.frame() %>%
    mutate(
      outcome  = outcome_label,
      age_grp  = factor(age_grp, levels = c("yng", "old"),
                        labels = c("Young", "Old")),
      condition = factor(condition, levels = c("low", "mod"),
                         labels = c("Low", "Moderate"))
    )
}

prep_emmeans_time <- function(df, outcome_label) {
  # For files that also have a time column
  prep_emmeans(df, outcome_label) %>%
    mutate(time = factor(time, levels = time_levels, labels = time_labels))
}

# ── Body composition ─────────────────────────────────────────────────────────
mv_dat  <- prep_emmeans_time(emm_mv,  "Muscle Volume\n(cm³)")
mt_dat  <- prep_emmeans_time(emm_mt,  "Muscle Thickness\n(mm)")
ublm_dat <- prep_emmeans_time(emm_ublm, "Upper-body Lean Mass\n(kg)")

# ── Strength ─────────────────────────────────────────────────────────────────
isom_dat   <- prep_emmeans_time(emm_isom,    "Isometric MVC\n(Nm)")
isok60_dat <- prep_emmeans_time(emm_isok60,  "60°/s\n(Nm)")
isok120_dat<- prep_emmeans_time(emm_isok120, "120°/s\n(Nm)")
isok240_dat<- prep_emmeans_time(emm_isok240, "240°/s\n(Nm)")
bc_dat     <- prep_emmeans_time(emm_bc,      "Bicep Curl MVC\n(Nm)")

# ── Exercise (volume load) ───────────────────────────────────────────────────
vl_dat <- emm_vl %>%
  as.data.frame() %>%
  mutate(
    age_grp   = factor(age_grp, levels = c("yng", "old"),
                       labels = c("Young", "Old")),
    condition = factor(condition, levels = c("low", "mod"),
                       labels = c("Low", "Moderate"))
  )


# ── Generic line plot (time on x, emmean ± 95% CI) ──────────────────────────
make_line_plot <- function(dat, ylab, title,
                           pd = position_dodge(width = 0.15)) {
  ggplot(dat,
         aes(x     = time,
             y     = emmean,
             colour = age_grp,
             linetype = condition,
             group  = interaction(age_grp, condition))) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  width = 0.1, linewidth = 0.35,
                  position = pd, show.legend = FALSE) +
    geom_line(linewidth = 0.6, position = pd) +
    geom_point(size = 1.5, position = pd) +
    scale_colour_manual(values = c(Young = col_yng, Old = col_old),
                        name = "Age group") +
    scale_linetype_manual(values = c(Low = lty_low, Moderate = lty_mod),
                          name = "Volume") +
    labs(title = title, x = NULL, y = ylab) +
    base_theme
}

# ── Bar plot for volume load (no time axis) ──────────────────────────────────
make_vl_plot <- function(dat, ylab = "Volume Load (kg x reps)", title = "Exercise Volume") {
  ggplot(dat,
         aes(x    = condition,
             y    = emmean,
             fill = age_grp,
             colour = age_grp)) +
    geom_col(position = position_dodge(0.7), width = 0.6,
             alpha = 0.7, linewidth = 0.3) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                  position = position_dodge(0.7),
                  width = 0.25, linewidth = 0.35) +
    scale_fill_manual(values   = c(Young = col_yng, Old = col_old)) +
    scale_colour_manual(values = c(Young = col_yng, Old = col_old)) +
    labs(title = title, x = "Volume condition", y = ylab) +
    base_theme +
    theme(legend.position = "none")
}


# ── Panel A: Study design ────────────────────────────────────────────────────
# Images
dxaimg     <- readPNG(here("figures/archive/dxa.fig.png"))
biopsyimg  <- readPNG(here("figures/archive/biopsy.fig.png"))
gtimg      <- readPNG(here("figures/archive/blood.vial.png"))
mriimg     <- readPNG(here("figures/archive/mri.png"))
strimg     <- readPNG(here("figures/archive/str.png"))
rtimg      <- readPNG(here("figures/archive/rt.fig.png"))
smallarrow <- readPNG(here("figures/archive/small.arrow.png"))
longarrow  <- readPNG(here("figures/archive/long.arrow.png"))

d.dat <- read_excel(here("data/design.dat.xlsx"), na = "NA")

line_size  <- 0.2
htextsize  <- 2.9
ltextsize  <- 2.3
textsize   <- 2
stextsize  <- 1.7

panel_A <- d.dat %>%
  ggplot(aes(time, tp)) +
  scale_y_continuous(limits = c(0,10), expand = c(0,0)) +
  scale_x_continuous(limits = c(0,24), expand = c(0,0)) +

  # Background
  geom_rect(aes(xmin=0,xmax=24,ymin=0,ymax=10),
            fill="#f0f4f8", color="#f0f4f8", linewidth=0, inherit.aes=FALSE) +
  annotate("segment", x=0,  xend=24,   y=5, yend=5,
           colour="#7f8c8d", linewidth=0.5) +
  annotate("segment", x=18.5, xend=18.5, y=10, yend=5,
           colour="#7f8c8d", linewidth=0.5) +

  # Timeline
  annotate("segment", x=.2,  xend=23.8, y=.5, yend=.5) +
  annotate("segment", x=.2,  xend=.2,   y=.3, yend=.7) +
  annotate("segment", x=3.5, xend=3.5,  y=.3, yend=.7) +
  annotate("segment", x=10.9,xend=10.9, y=.3, yend=.7) +
  annotate("segment", x=13.1,xend=13.1, y=.3, yend=.7) +
  annotate("segment", x=20,  xend=20,   y=.3, yend=.7) +
  annotate("segment", x=23.8,xend=23.8, y=.3, yend=.7) +

  # Recruitment heading
  annotate("text", x=9.5, y=9.7, label="Recruitment", size=htextsize) +

  # Screening
  annotate("text", x=1.4, y=8.25, label="Screening",          size=ltextsize) +
  annotate("text", x=1.4, y=7.75, label="Inc./Ex. criteria",  size=ltextsize) +
  annotate("segment", x=0.2, xend=10, y=7.25, yend=7.25) +
  draw_image(smallarrow, x=2.5,  y=6.77, scale=0.5) +

  # Inclusion
  annotate("text", x=5, y=8.25, label="Inclusion (n = )",  size=ltextsize) +
  annotate("text", x=5, y=7.75, label="Informed consent",   size=ltextsize) +
  draw_image(smallarrow, x=6.5,  y=6.77, scale=0.5) +

  # Age stratification
  annotate("text", x=8.5, y=8.25, label="Age",             size=ltextsize) +
  annotate("text", x=8.5, y=7.75, label="stratification",  size=ltextsize) +
  annotate("segment", x=10, xend=11.5, y=8.4, yend=8.4) +
  annotate("segment", x=10, xend=10,   y=7.25, yend=8.4) +
  draw_image(smallarrow, x=11,  y=7.905, scale=0.5) +
  annotate("segment", x=10, xend=11.5, y=6.3, yend=6.3) +
  annotate("segment", x=10, xend=10,   y=7.25, yend=6.3) +
  draw_image(smallarrow, x=11,  y=5.8,  scale=0.5) +

  # Groups
  annotate("text", x=12.5, y=8.4, label="<30 yrs", size=ltextsize) +
  annotate("text", x=12.5, y=6.3, label=">70 yrs", size=ltextsize) +

  # Randomization
  annotate("text", x=14.75, y=7.35, label="Randomization", size=ltextsize) +
  annotate("segment", x=13.5, xend=16, y=8.4, yend=8.4) +
  draw_image(smallarrow, x=15.5, y=7.905, scale=0.5) +
  annotate("segment", x=13.5, xend=16, y=6.3, yend=6.3) +
  draw_image(smallarrow, x=15.5, y=5.8,  scale=0.5) +

  # Volumes
  annotate("text", x=16.75, y=8.8,  label="LV",  size=ltextsize) +
  annotate("text", x=16.75, y=8.1,  label="MV",  size=ltextsize) +
  annotate("text", x=16.75, y=7,    label="LV",  size=ltextsize) +
  annotate("text", x=16.75, y=6.3,  label="MV",  size=ltextsize) +
  annotate("text", x=16.75, y=5.7,  label="CON", size=ltextsize) +

  # Symbol legend (right panel)
  annotate("text", x=20.5, y=9.6, label="DXA/US", size=textsize) +
  annotate("text", x=20.5, y=8.9, label="Biopsy",  size=textsize) +
  annotate("text", x=20.5, y=7.9, label="GT",      size=textsize) +
  annotate("text", x=20.5, y=7,   label="MRI",     size=textsize) +
  annotate("text", x=20.5, y=6.2, label="STR",     size=textsize) +
  annotate("text", x=20.5, y=5.4, label="RT",      size=textsize) +
  draw_image(dxaimg,    x=21.5, y=9.1,  scale=0.5) +
  draw_image(biopsyimg, x=21.5, y=8.26, scale=0.6) +
  draw_image(gtimg,     x=21.5, y=7.42, scale=0.6) +
  draw_image(mriimg,    x=21.5, y=6.58, scale=0.6) +
  draw_image(strimg,    x=21.5, y=5.74, scale=0.6) +
  draw_image(rtimg,     x=21.5, y=4.9,  scale=0.6) +

  # Baseline block
  annotate("text", x=2,   y=.2,  label="Week 1: Baseline", size=textsize) +
  annotate("text", x=.6,  y=.7,  label="Pre 1",            size=stextsize) +
  annotate("text", x=1.75,y=.7,  label="Pre 2",            size=stextsize) +
  annotate("text", x=2.9, y=.7,  label="Pre 3",            size=stextsize) +
  annotate("text", x=3.9, y=.7,  label="Pre 4",            size=stextsize) +
  draw_image(dxaimg,    x=.1,  y=3.4, scale=.9) +
  draw_image(biopsyimg, x=.1,  y=2.2, scale=.9) +
  draw_image(gtimg,     x=.1,  y=1,   scale=.9) +
  draw_image(strimg,    x=1.2, y=1,   scale=.9) +
  draw_image(mriimg,    x=2.3, y=1,   scale=.9) +

  # Intervention block
  annotate("text", x=12,  y=4.5, label="Week 6: Mid",        size=textsize) +
  annotate("text", x=7.5, y=3.2, label="2x/week for 5 weeks", size=textsize) +
  annotate("text", x=16.3,y=3.2, label="3x/week for 5 weeks", size=textsize) +
  annotate("text", x=8,   y=.2,  label="Week 3",              size=textsize) +
  annotate("text", x=12,  y=.2,  label="Week 5-6: Mid",       size=textsize) +
  annotate("text", x=11.4,y=.7,  label="Mid 1/2",             size=stextsize) +
  annotate("text", x=12.5,y=.7,  label="Mid 3",               size=stextsize) +
  draw_image(strimg,    x=3.4,  y=1,   scale=.9) +
  draw_image(rtimg,     x=4.3,  y=3,   scale=1.2) +
  draw_image(longarrow, x=7.7,  y=2.3, scale=2, width=8) +
  draw_image(biopsyimg, x=7.5,  y=.8,  scale=1) +
  draw_image(strimg,    x=10.9, y=1,   scale=1) +
  draw_image(dxaimg,    x=11.9, y=1,   scale=1) +
  draw_image(rtimg,     x=13.2, y=3,   scale=1.2) +
  draw_image(strimg,    x=19.2, y=1,   scale=1) +

  # Post block
  annotate("text", x=22,  y=.2,  label="Week 12: Post", size=textsize) +
  annotate("text", x=20.8,y=.7,  label="Post 1",        size=stextsize) +
  annotate("text", x=22,  y=.7,  label="Post 2",        size=stextsize) +
  annotate("text", x=23.2,y=.7,  label="Post 3",        size=stextsize) +
  draw_image(dxaimg,    x=20.3, y=2.2, scale=1) +
  draw_image(biopsyimg, x=20.3, y=1,   scale=1) +
  draw_image(gtimg,     x=21.45,y=1,   scale=1) +
  draw_image(mriimg,    x=22.6, y=1,   scale=1) +

  theme_void()


# ── Panel D: Exercise volume load ────────────────────────────────────────────
panel_D <- make_vl_plot(vl_dat,
                        ylab  = "Volume Load (kg·reps)",
                        title = "Exercise Volume")


# ── Panel B: Body composition + exercise volume ──────────────────────────────
p_mv  <- make_line_plot(mv_dat,  "Muscle Volume (cm³)",  "Muscle Volume")
p_mt  <- make_line_plot(mt_dat,  "Muscle Thickness (mm)", "Muscle Thickness")
p_ublm <- make_line_plot(ublm_dat, "Upper-body Lean Mass (kg)",   "Upper-body Lean Mass")

panel_B <- plot_grid(p_mv, p_mt, p_ublm, panel_D,
                     nrow = 1, align = "h",
                     rel_widths = c(1, 1, 1, 0.75))


# ── Panel C: Strength ────────────────────────────────────────────────────────
p_isom    <- make_line_plot(isom_dat,    "Peak Torque (Nm)", "Isometric MVC")
p_isok60  <- make_line_plot(isok60_dat,  "Peak Torque (Nm)", "60°/s")
p_isok120 <- make_line_plot(isok120_dat, "Peak Torque (Nm)", "120°/s")
p_isok240 <- make_line_plot(isok240_dat, "Peak Torque (Nm)", "240°/s")
p_bc      <- make_line_plot(bc_dat,      "Peak Torque (Nm)", "Bicep Curl MVC")

panel_C <- plot_grid(p_isom, p_isok60, p_isok120, p_isok240, p_bc,
                     nrow = 1, align = "hv")




# ── Shared legend ────────────────────────────────────────────────────────────
# Build a dummy plot to extract a clean legend from
legend_plot <- ggplot(isom_dat,
                      aes(x = time, y = emmean,
                          colour   = age_grp,
                          linetype = condition,
                          group    = interaction(age_grp, condition))) +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c(Young = col_yng, Old = col_old),
                      name = "Age group") +
  scale_linetype_manual(values = c(Low = lty_low, Moderate = lty_mod),
                        name = "Volume") +
  guides(
    colour   = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "bottom",
        legend.box       = "horizontal",
        legend.key.width = unit(1.2, "cm"))

shared_legend <- cowplot::get_plot_component(legend_plot, "guide-box", return_all = TRUE)

#  the issue was that get_legend() in your version of cowplot has return_all hardcoded to FALSE and doesn't expose it as an argument, so you need to go one level deeper with get_plot_component() directly



#fig1 <- plot_grid(
#  plot_grid(panel_A, labels = "A", label_size = 9),
#  plot_grid(panel_B, labels = "B", label_size = 9),
#  plot_grid(panel_C, labels = "C", label_size = 9),
#  shared_legend,
#  ncol        = 1,
#  rel_heights = c(1.6, 1, 1, 0.15)
#)
#fig1 <- fig1 + theme(plot.background = element_rect(fill = "#f0f4f8", colour = #NA))

#fig1

fig1 <- plot_grid(
  plot_grid(panel_A, labels = "A", label_size = 9, scale = 1),
  plot_grid(panel_B, labels = "B", label_size = 9),
  plot_grid(panel_C, labels = "C", label_size = 9),
  shared_legend,
  ncol        = 1,
  rel_heights = c(1.6, 1, 1, 0.15)
)

fig1



ggsave(
  filename = here("figures/fig1_draft2.pdf"),
  plot     = fig1,
  device   = "pdf",
  width    = 174,
  height   = 234,
  units    = "mm",
  dpi      = 1200,
  bg       = "#f0f4f8"
)

ggsave(
  filename = here("figures/fig1_draft2.png"),
  plot     = fig1,
  device   = "png",
  width    = 174,
  height   = 234,
  units    = "mm",
  dpi      = 300,
  bg       = "#f0f4f8"
)


