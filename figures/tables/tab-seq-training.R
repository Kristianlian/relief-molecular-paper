## RNA-seq — training and volume effects, summary tables

library(tidyverse)
library(seqwrap)
library(gt)

# --- Load model results ---
m2s_summaries <- readRDS("data-out/m2-summaries.RDS")

contrast_order <- c("young_t3", "old_t3", "young_old_t3",
                    "mod_low_t3", "young_volume_t3", "old_volume_t3",
                    "young_old_volume_t3")

contrast_labels <- c(
  young_t3             = "Young: Baseline to t3",
  old_t3               = "Old: Baseline to t3",
  young_old_t3         = "Young vs. Old (training response)",
  mod_low_t3           = "Mod vs. Low (averaged over age)",
  young_volume_t3      = "Young: Mod vs. Low",
  old_volume_t3        = "Old: Mod vs. Low",
  young_old_volume_t3  = "Volume effect: Young vs. Old"
)

# --- Gene-level summary, all 7 contrasts ---

training_genes <- m2s_summaries |>
  filter(term %in% contrast_order) |>
  mutate(p.adj = p.adjust(p.value, method = "BH"), .by = term) |>
  summarise(.by = term,
            n_sig_5pct           = sum(p.adj < 0.05, na.rm = TRUE),
            n_sig_1pct           = sum(p.adj < 0.01, na.rm = TRUE),
            median_log2_estimate = median(abs(estimate) / log(2), na.rm = TRUE)) |>
  mutate(term = factor(term, levels = contrast_order)) |>
  arrange(term) |>
  mutate(label = contrast_labels[as.character(term)]) |>
  select(label, n_sig_5pct, n_sig_1pct, median_log2_estimate)

table_rnaseq_training_genes <- training_genes |>
  gt() |>
  fmt_number(columns = median_log2_estimate, decimals = 3) |>
  cols_label(
    label                = "Contrast",
    n_sig_5pct           = "Significant (FDR < 5%)",
    n_sig_1pct           = "Significant (FDR < 1%)",
    median_log2_estimate = "Median |log2 FC|"
  ) |>
  tab_header(title = "RNA-seq: training and volume effects")

table_rnaseq_training_genes

# --- Top enriched pathways per contrast (top 3 by FDR) ---

gsea_files <- c(
  young_t3      = "data-out/gsea-young-t3.RDS",
  old_t3        = "data-out/gsea-old-t3.RDS",
  old_volume_t3 = "data-out/gsea-old-volume-t3.RDS",
  young_old_t3  = "data-out/gsea-young-old-t3.RDS"
)

training_pathways <- map_dfr(names(gsea_files), function(nm) {
  res <- readRDS(gsea_files[nm])
  data.frame(res$gse_results) |>
    select(ID, Description, NES, p.adjust) |>
    arrange(p.adjust) |>
    slice_head(n = 3) |>
    mutate(contrast = contrast_labels[nm], .before = 1)
})

table_rnaseq_training_pathways <- training_pathways |>
  gt(groupname_col = "contrast") |>
  fmt_number(columns = NES, decimals = 2) |>
  fmt_scientific(columns = p.adjust, decimals = 1) |>
  cols_label(
    ID          = "GO ID",
    Description = "Pathway",
    NES         = "NES",
    p.adjust    = "FDR"
  ) |>
  tab_header(title = "RNA-seq: training and volume effects — top enriched pathways")

table_rnaseq_training_pathways


saveRDS(table_rnaseq_training_genes,    "figures/tables/tab-rnaseq-training-genes.RDS")
saveRDS(table_rnaseq_training_pathways, "figures/tables/tab-rnaseq-training-pathways.RDS")
gtsave(table_rnaseq_training_genes,    "figures/tables/tab-rnaseq-training-genes.html")
gtsave(table_rnaseq_training_pathways, "figures/tables/tab-rnaseq-training-pathways.html")
