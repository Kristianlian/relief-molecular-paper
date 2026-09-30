# Figure 3 - muscular strength


library(tidyverse)
library(cowplot)

# ── Shared aesthetics (same as Figure 1/2) ──────────────────────────────────
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
pred_humac_120  <- readRDS("./data/data-gen/pred_humac_120.rds")
contr_humac_120 <- readRDS("./data/data-gen/contrast_humac_120.rds")

pred_humac_240  <- readRDS("./data/data-gen/pred_humac_240.rds")
contr_humac_240 <- readRDS("./data/data-gen/contrast_humac_240.rds")

pred_bc  <- readRDS("./data/data-gen/pred_bc.rds")
contr_bc <- readRDS("./data/data-gen/contrast_bc.rds")

pred_1rm  <- readRDS("./data/data-gen/pred_1rm.rds")
contr_1rm <- readRDS("./data/data-gen/contrast_1rm.rds")

# ── HUMAC-style outcomes: tx-level (sex-weighted), pre/post only, additive scale ──
# (same pattern as Figure 1's isometric/60 panels)
make_humac_pred_plot <- function(dat, contr_dat, ylab) {
  contr_dat <- contr_dat %>%
    add_facet_grp("tx") %>%
    mutate(
      credible = lower95 > 0 | upper95 < 0,
      facet_label = if_else(credible, paste0(as.character(facet_grp), " **"), as.character(facet_grp))
    )
  
  humac_labels <- setNames(
    contr_dat$facet_label[match(facet_order, contr_dat$facet_grp)],
    tx_levels
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
    scale_linetype_manual(values = c(Low = "solid", Moderate = "dashed", Control = "dotted"), name = "Volume") +
    labs(x = NULL, y = ylab) +
    base_theme
}

p_120 <- make_humac_pred_plot(pred_humac_120, contr_humac_120, "Peak Torque (Nm)\n120 deg/s")
p_240 <- make_humac_pred_plot(pred_humac_240, contr_humac_240, "Peak Torque (Nm)\n240 deg/s")

# ── Bicep curl / 1RM: sex-split, 3 timepoints, ratio scale (same as Figure 2) ──
extract_within_delta <- function(contr_df) {
  contr_df %>%
    filter(str_detect(hypothesis, "_delta_(mid|midpost|post)$")) %>%
    mutate(
      tx = str_remove(hypothesis, "_delta_(mid|midpost|post)$"),
      tx = factor(tx, levels = tx_levels),
      facet_grp = factor(parse_tx(tx), levels = facet_order)
    ) %>%
    filter(!is.na(tx)) %>%
    select(tx, facet_grp, timepoint, credible)
}

make_pred_plot_3tp <- function(pred_df, contr_df, ylab) {
  pred_df <- pred_df %>%
    mutate(
      tx = factor(tx, levels = tx_levels),
      time = factor(time, levels = c("pre", "mid", "post"), labels = c("Pre", "Mid", "Post")),
      time_num = as.numeric(time)
    )
  
  cred <- extract_within_delta(contr_df)
  
  seg_marks <- cred %>%
    filter(timepoint %in% c("mid", "midpost"), credible == "Yes") %>%
    mutate(x = if_else(timepoint == "mid", 1.5, 2.5))
  
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

p_1rm <- make_pred_plot_3tp(pred_1rm, contr_1rm, "1RM Leg Press (kg)")
p_bc  <- make_pred_plot_3tp(pred_bc,  contr_bc,  "Bicep Curl MVC (N)")

# ── Shared legends (two: age/volume for HUMAC, sex for bicep/1RM) ──────────
legend_humac <- ggplot(pred_humac_120 %>% add_facet_grp("tx"),
                       aes(time, m, colour = age_grp, linetype = condition, group = tx)) +
  geom_line() + geom_point() +
  scale_colour_manual(values = c(Young = col_yng, Old = col_old), name = "Age group") +
  scale_linetype_manual(values = c(Low = "solid", Moderate = "dashed", Control = "dotted"), name = "Volume") +
  theme_minimal(base_size = 8) +
  theme(legend.position = "bottom", legend.box = "horizontal")
shared_legend_humac <- cowplot::get_plot_component(legend_humac, "guide-box", return_all = TRUE)

legend_sex <- ggplot(pred_bc, aes(time, m, colour = sex, group = paste(tx, sex))) +
  geom_line() + geom_point() +
  scale_colour_discrete(name = "Sex", labels = c("Female", "Male")) +
  theme_minimal(base_size = 8) +
  theme(legend.position = "bottom", legend.box = "horizontal")
shared_legend_sex <- cowplot::get_plot_component(legend_sex, "guide-box", return_all = TRUE)

# ── Assemble ─────────────────────────────────────────────────────────────────
fig3 <- plot_grid(
  plot_grid(p_120, labels = "A", label_size = 9),
  plot_grid(p_240, labels = "B", label_size = 9),
  shared_legend_humac,
  plot_grid(p_1rm, labels = "C", label_size = 9),
  plot_grid(p_bc,  labels = "D", label_size = 9),
  shared_legend_sex,
  ncol = 1,
  rel_heights = c(1, 1, 0.12, 1, 1, 0.12)
)

saveRDS(fig3, ("./figures/figure-3.RDS"))

ggsave(
  filename = "./figures/figure-3.png",
  plot = fig3,
  device = "png",
  width = 174,
  height = 200,
  units = "mm",
  dpi = 300,
  bg = "white"
)
