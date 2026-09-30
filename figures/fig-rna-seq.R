# RNA-seq figures
# Standalone figure generation for the RNA-seq results.
# Reads cached model fits and GSEA results.

library(tidyverse)
library(seqwrap)   
source("./figures/plot-helpers.R")

# Load cached model results
m2r <- readRDS("data-out/m2-rnaseq.RDS")
m2s <- seqwrap_summarise(m2r, verbose = FALSE)

# Load cached GSEA results
gsearesults_ageyoung      <- readRDS("data-out/gsea-ageyoung.RDS")
gsearesults_young_old_t3  <- readRDS("data-out/gsea-young-old-t3.RDS")
#gsearesults_young_t3      <- readRDS("data-out/gsea-young-t3.RDS")
#gsearesults_old_t3        <- readRDS("data-out/gsea-old-t3.RDS")
#gsearesults_old_volume_t3 <- readRDS("data-out/gsea-old-volume-t3.RDS")


# Figure: baseline age differences (Young vs. Old)
p1_baseline <- volcano_plot(m2s$summaries, 
                            term_name = "ageyoung", 
                            label_thresh = 40, 
                            y_max = 90,
                            x_label = "Estimate (log<sub>2</sub> difference Young - Old)")

p2_baseline <- gse_plot(gsearesults_ageyoung$gse_results, 
                        genesets = c("GO:0006119", "GO:0006958", "GO:0031507"), 
                        colscale = c("gray80", "#DE8F05", "#009E73", "#0173B2", "#b24001"))

fig_age_diff <- cowplot::plot_grid(p1_baseline, p2_baseline, 
                                   ncol = 2, rel_widths = c(0.7, 1),
                                   labels = c("A", "B"))




# Figure: Training effect differences
p1_interaction <- volcano_plot(m2s$summaries, 
                               term_name = "young_old_t3", 
                               label_thresh = Inf, 
                               y_max = 6,
                               x_label = "Estimate (log<sub>2</sub>, Young − Old training response)",
                               title = NULL)


p2_interaction <- gse_plot(gsearesults_young_old_t3$gse_results, 
                           genesets = c("GO:0008380", "GO:0001525", "GO:0009060"), 
                           colscale = c("gray80", "#DE8F05", "#009E73", "#0173B2"))

fig_training_diff <- cowplot::plot_grid(p1_interaction, p2_interaction,
                                        ncol = 2, rel_widths = c(0.7, 1),
                                        labels = c("C", "D"))


p_rnaseq <- fig_age_diff / fig_training_diff 

p_rnaseq

saveRDS(p_rnaseq, "figures/fig-rnaseq.RDS")

ggsave("./figures/fig-rnaseq-combined.png", p_rnaseq,
       width = 10, height = 10, dpi = 300)




