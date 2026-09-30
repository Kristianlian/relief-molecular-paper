# Figure 2: Muscle comp (muscle thickness and upper body lean mass)


library(tidyverse)
library(cowplot)

# ── Shared aesthetics (reuse from Figure 1 script) ──────────────────────────
col_yng <- "#2166ac"
col_old <- "#d6604d"

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
tx_levels <- c("int_old_low", "int_old_mod", "int_yng_low", "int_yng_mod", "con_old_con")

parse_tx <- function(tx) {
  case_when(
    tx == "int_old_low" ~ "Old, Low volume",
    tx == "int_old_mod" ~ "Old, Mod volume",
    tx == "int_yng_low" ~ "Young, Low volume",
    tx == "int_yng_mod" ~ "Young, Mod volume",
    tx == "con_old_con" ~ "Old, Control"
  )
}

# ── Read data ────────────────────────────────────────────────────────────────
pred_mt  <- readRDS("./data/data-gen/pred_mt.rds")
contr_mt <- readRDS("./data/data-gen/contrast_mt.rds")

pred_ublm  <- readRDS("./data/data-gen/pred_ublm.rds")
contr_ublm <- readRDS("./data/data-gen/contrast_ublm.rds")

# ── Pull the 5 within-condition delta rows per timepoint from a contrast df ─
extract_within_delta <- function(contr_df) {
  contr_df %>%
    filter(str_detect(hypothesis, "_delta_(mid|midpost|post)$")) %>%
    mutate(
      tx = str_remove(hypothesis, "_delta_(mid|midpost|post)$"),
      tx = factor(tx, levels = tx_levels),
      facet_grp = factor(parse_tx(tx), levels = facet_order)
    ) %>%
    filter(!is.na(tx)) %>%   # drops any non-within-condition hypothesis that still matched
    select(tx, facet_grp, timepoint, credible)
}

# ── Build a plot for one outcome, annotating segment-level credibility ──────
make_pred_plot_3tp <- function(pred_df, contr_df, ylab) {
  pred_df <- pred_df %>%
    mutate(
      tx = factor(tx, levels = tx_levels),
      time = factor(time, levels = c("pre", "mid", "post"), labels = c("Pre", "Mid", "Post")),
      time_num = as.numeric(time)
    )
  
  cred <- extract_within_delta(contr_df)
  
  # Segment-level marks (pre-mid, mid-post) keep "*"
  seg_marks <- cred %>%
    filter(timepoint %in% c("mid", "midpost"), credible == "Yes") %>%
    mutate(x = if_else(timepoint == "mid", 1.5, 2.5))
  
  # Facet-label mark (overall pre-post) now uses "†" instead of "*"
  facet_cred <- cred %>%
    filter(timepoint == "post") %>%
    mutate(facet_label = if_else(credible == "Yes", paste0(as.character(facet_grp), " **"), as.character(facet_grp)))
  
  facet_labels <- setNames(facet_cred$facet_label, facet_cred$tx)
  
  y_max <- pred_df %>% summarise(.by = tx, ymax = max(upr95))
  seg_marks <- seg_marks %>% left_join(y_max, by = "tx")
  
  ggplot(pred_df, aes(time_num, m, colour = sex, group = paste(tx, sex))) +
    geom_errorbar(aes(ymin = lwr95, ymax = upr95), width = 0.08, linewidth = 0.35, show.legend = FALSE) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    geom_text(data = seg_marks, aes(x = x, y = ymax * 1.05, label = "*"),
              inherit.aes = FALSE, size = 3, colour = "black") +
    scale_x_continuous(breaks = 1:3, labels = c("Pre", "Mid", "Post"), expand = expansion(mult = 0.15)) +
    facet_wrap(~tx, nrow = 1, labeller = labeller(tx = facet_labels)) +
    scale_colour_discrete(name = "Sex", labels = c("Female", "Male")) +
    labs(x = NULL, y = ylab) +
    base_theme
}


p_mt   <- make_pred_plot_3tp(pred_mt,   contr_mt,   "Muscle Thickness (mm)")
p_ublm <- make_pred_plot_3tp(pred_ublm, contr_ublm, "Upper-body Lean Mass (kg)")

# ── Shared legend (Sex) ──────────────────────────────────────────────────────
legend_plot <- ggplot(pred_mt, aes(time, m, colour = sex, group = paste(tx, sex))) +
  geom_line() +
  geom_point() +
  scale_colour_discrete(name = "Sex", labels = c("Female", "Male")) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "bottom", legend.box = "horizontal")

shared_legend <- cowplot::get_plot_component(legend_plot, "guide-box", return_all = TRUE)

fig2 <- plot_grid(
  plot_grid(p_mt,   labels = "A", label_size = 9),
  plot_grid(p_ublm, labels = "B", label_size = 9),
  shared_legend,
  ncol = 1,
  rel_heights = c(1, 1, 0.12)
)

ggsave(
  filename = "figures/figure-2.png",
  plot = fig2,
  device = "png",
  width = 174,
  height = 140,
  units = "mm",
  dpi = 300,
  bg = "white"
)
