# RNA-seq figures
# Standalone figure generation for the RNA-seq results.
# Reads cached model fits and GSEA results.

library(tidyverse)
library(seqwrap)
source("./figures/plot-helpers.R")

dir.create("figures", showWarnings = FALSE)

# Load cached model results
m2r <- readRDS("data-out/m2-rnaseq.RDS")
m2s <- seqwrap_summarise(m2r, verbose = FALSE)

# Load cached GSEA results
gsearesults_ageyoung      <- readRDS("data-out/gsea-ageyoung.RDS")
gsearesults_young_old_t3  <- readRDS("data-out/gsea-young-old-t3.RDS")
#gsearesults_young_t3      <- readRDS("data-out/gsea-young-t3.RDS")
#gsearesults_old_t3        <- readRDS("data-out/gsea-old-t3.RDS")
#gsearesults_old_volume_t3 <- readRDS("data-out/gsea-old-volume-t3.RDS")

# Common settings for all panels. Figures are saved at 16 cm width, so
# text is set in points for that size (base 8 pt, gene labels about 6.5 pt).
panel_widths <- c(0.75, 1)     # volcano : GSEA panel. Increase the first value
                               # if the volcano x-axis label still clips.

# ============================================================
# Baseline age differences (Young vs. Old)
# ============================================================
p1_baseline <- volcano_plot(m2s$summaries,
                            term_name = "ageyoung",
                            label_thresh = 40,
                            label_n = 10,      # label only the 10 most significant genes
                            y_max = 95,
                            x_label = "Estimate (log<sub>2</sub> difference,<br>Young − Old)")

p2_baseline <- gse_plot(gsearesults_ageyoung$gse_results,
                        genesets = c("GO:0006119", "GO:0006958", "GO:0031507"),
                        colscale = c("gray80", "#DE8F05", "#009E73", "#0173B2", "#b24001"))

fig_age_diff <- cowplot::plot_grid(p1_baseline, p2_baseline,
                                   ncol = 2, rel_widths = panel_widths,
                                   labels = c("A", "B"),
                                   label_size = 10, label_fontface = "bold")

# ============================================================
# Training effect differences (age x time interaction)
# ============================================================
p1_interaction <- volcano_plot(m2s$summaries,
                               term_name = "young_old_t3",
                               label_thresh = Inf,      # no gene labels
                               y_max = 6,
                               x_label = "Estimate (log<sub>2</sub>), Young − Old<br>training response")

p2_interaction <- gse_plot(gsearesults_young_old_t3$gse_results,
                           genesets = c("GO:0008380", "GO:0001525", "GO:0009060"),
                           colscale = c("gray80", "#DE8F05", "#009E73", "#0173B2"))

# Labels C-D for the combined figure, A-B for the stand-alone training figure
fig_training_diff <- cowplot::plot_grid(p1_interaction, p2_interaction,
                                        ncol = 2, rel_widths = panel_widths,
                                        labels = c("C", "D"),
                                        label_size = 10, label_fontface = "bold")

fig_training_diff_ab <- cowplot::plot_grid(p1_interaction, p2_interaction,
                                           ncol = 2, rel_widths = panel_widths,
                                           labels = c("A", "B"),
                                           label_size = 10, label_fontface = "bold")

# ============================================================
# Combined figure (cowplot only; no patchwork needed)
# ============================================================
p_rnaseq <- cowplot::plot_grid(fig_age_diff, fig_training_diff, ncol = 1)

p_rnaseq

# ============================================================
# Save
# ============================================================
saveRDS(p_rnaseq, "figures/fig-rnaseq.RDS")
ggsave("./figures/fig-rnaseq-combined.png", p_rnaseq,
       width = 16, height = 18, units = "cm", dpi = 300, bg = "white")

saveRDS(fig_age_diff, "figures/fig-rnaseq-baseline.RDS")
ggsave("./figures/fig-rnaseq-baseline.png", fig_age_diff,
       width = 16, height = 9, units = "cm", dpi = 300, bg = "white")

saveRDS(fig_training_diff_ab, "figures/fig-rnaseq-training.RDS")
ggsave("./figures/fig-rnaseq-training.png", fig_training_diff_ab,
       width = 16, height = 9, units = "cm", dpi = 300, bg = "white")
