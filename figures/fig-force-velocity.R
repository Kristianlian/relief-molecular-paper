## Humac fig

library(tidyverse)
library(cowplot)

all_data        <- readRDS("data/data-gen/torque_hillcurve_data.rds")
speed_table_nm  <- readRDS("data/data-gen/torque_speed_contrasts_nm.rds")
speed_table_pct <- readRDS("data/data-gen/torque_speed_contrasts_pct.rds")
within_table_nm  <- readRDS("data/data-gen/torque_within_nm.rds")
within_table_pct <- readRDS("data/data-gen/torque_within_pct.rds")
contrast_curves <- readRDS("data/data-gen/torque_contrast_curves.rds")


speed_lookup <- tibble::tibble(
  speed_label = factor(c("Isometric", "60\u00b0/s", "120\u00b0/s", "240\u00b0/s"),
                       levels = c("Isometric", "60\u00b0/s", "120\u00b0/s", "240\u00b0/s")),
  sp = c(0, 60/240, 120/240, 240/240)
)


vol_colors <- c("Control" = "grey40", "Low volume" = "#1b9e77", "Moderate volume" = "#d95f02")
av_breaks <- c(0, 60/240, 120/240, 1)
av_labels <- c("0", "60", "120", "240")


y_range <- range(all_data$m)

theme_fv <- theme_minimal(base_size = 10) +
  theme(
    panel.grid   = element_blank(),
    axis.line    = element_line(color = "black", linewidth = 0.3),
    axis.ticks   = element_line(color = "black", linewidth = 0.3),
    legend.key   = element_blank(),
    plot.title   = element_text(face = "bold", hjust = 0)
  )

# --- panels 1-3: raw predicted curves, same style as before ---
p_young <- all_data |>
  filter(age == "yng") |>
  ggplot(aes(sp, m, color = cond_label, linetype = time)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = vol_colors, drop = TRUE) +
  scale_x_continuous(breaks = av_breaks, labels = av_labels) +
  scale_y_continuous(limits = y_range) +
  labs(title = "Young (intervention)", x = NULL, y = "Predicted peak torque (Nm)",
       color = "Group", linetype = "Timepoint") 

p_old_int <- all_data |>
  filter(age == "old", allocation == "int") |>
  ggplot(aes(sp, m, color = cond_label, linetype = time)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = vol_colors, drop = TRUE) +
  scale_x_continuous(breaks = av_breaks, labels = av_labels) +
  scale_y_continuous(limits = y_range) +
  labs(title = "Old (intervention)", x = NULL, y = NULL,
       color = "Group", linetype = "Timepoint")

p_control <- all_data |>
  filter(tx == "con_old_con") |>
  ggplot(aes(sp, m, linetype = time)) +
  geom_line(linewidth = 0.8, color = vol_colors[["Control"]]) +
  scale_x_continuous(breaks = av_breaks, labels = av_labels) +
  scale_y_continuous(limits = y_range) +
  labs(title = "Old (control)", x = "Angular velocity (\u00b0/s)", y = "Predicted peak torque (Nm)",
       linetype = "Timepoint") 

# --- panel 4: pooled contrasts at the 4 tested speeds, point + 95% CI ---
contrast_points <- speed_table_nm |>
  filter(contrast %in% c("Age (pooled across volume)",
                         "Intervention vs control (pooled across volume, older)"),
         timepoint %in% c("mid", "post")) |>
  left_join(speed_lookup, by = "speed_label") |>
  mutate(contrast_label = recode(contrast,
                                 "Age (pooled across volume)" = "Young vs old",
                                 "Intervention vs control (pooled across volume, older)" = "Intervention vs control"),
         timepoint = factor(timepoint, levels = c("mid", "post"), labels = c("Mid", "Post")))

p_contrast <- contrast_points |>
  ggplot(aes(sp, mean, color = contrast_label, shape = timepoint,
             group = interaction(contrast_label, timepoint))) +
  geom_hline(yintercept = 0, linetype = 2, color = "grey40") +
  geom_pointrange(aes(ymin = lower95, ymax = upper95),
                  position = position_dodge(width = 0.12), linewidth = 0.8, size = 0.5) +
  scale_x_continuous(breaks = av_breaks, labels = av_labels) +
  labs(title = "Contrasts (\u0394 vs pre)", x = "Angular velocity (\u00b0/s)",
       y = "\u0394 Peak torque (Nm)", color = NULL, shape = "Timepoint")



p_young    <- p_young    + theme_fv
p_old_int  <- p_old_int  + theme_fv
p_control  <- p_control  + theme_fv
p_contrast <- p_contrast + theme_fv

legend_group    <- cowplot::get_legend(p_young + theme(legend.box = "vertical"))
legend_contrast <- cowplot::get_legend(p_contrast + theme(legend.box = "vertical"))

p_young    <- p_young    + theme(legend.position = "none")
p_old_int  <- p_old_int  + theme(legend.position = "none")
p_control  <- p_control  + theme(legend.position = "none")
p_contrast <- p_contrast + theme(legend.position = "none")

panels  <- plot_grid(p_young, p_old_int, p_control, p_contrast,
                     ncol = 2, align = "hv", axis = "tblr")
legends <- plot_grid(legend_group, legend_contrast, ncol = 1)

fig_force_velocity <- plot_grid(panels, legends, ncol = 2, rel_widths = c(1, 0.22))



saveRDS(fig_force_velocity, "./figures/fig-force-velocity.RDS")

ggsave(
  filename = "./figures/fig-force-velocity.png",
  plot = fig_force_velocity,
  device = "png",
  width = 180,
  height = 120,
  units = "mm",
  dpi = 300,
  bg = "white"
)




