## RNA-seq — baseline age difference, summary tables

library(tidyverse)
library(seqwrap)
library(gt)

# --- Load model results ---
m2s_summaries <- readRDS("data-out/m2-summaries.RDS")

gsearesults_baseline <- readRDS("data-out/gsea-ageyoung.RDS")

# --- Gene-level summary ---

baseline_genes <- m2s_summaries |>
  filter(term == "ageyoung") |>
  mutate(p.adj = p.adjust(p.value, method = "BH")) |>
  summarise(
    n_tested       = n(),
    n_sig_5pct     = sum(p.adj < 0.05, na.rm = TRUE),
    n_higher_young = sum(p.adj < 0.05 & estimate > 0, na.rm = TRUE),
    n_higher_old   = sum(p.adj < 0.05 & estimate < 0, na.rm = TRUE)
  )

table_rnaseq_baseline_genes <- baseline_genes |>
  gt() |>
  cols_label(
    n_tested       = "Genes tested",
    n_sig_5pct     = "Significant (FDR < 5%)",
    n_higher_young = "Higher in young",
    n_higher_old   = "Higher in old"
  ) |>
  tab_header(title = "RNA-seq: baseline age difference (Young vs. Old)")

table_rnaseq_baseline_genes

# --- Top enriched pathways, matching the published figure's curated set ---

baseline_pathways <- data.frame(gsearesults_baseline$gse_results) |>
  filter(ID %in% c("GO:0006119", "GO:0006958", "GO:0031507")) |>
  select(ID, Description, NES, p.adjust) |>
  mutate(direction = if_else(NES > 0, "Higher in young", "Higher in old")) |>
  arrange(p.adjust)

table_rnaseq_baseline_pathways <- baseline_pathways |>
  gt() |>
  fmt_number(columns = NES, decimals = 2) |>
  fmt_scientific(columns = p.adjust, decimals = 1) |>
  cols_label(
    ID          = "GO ID",
    Description = "Pathway",
    NES         = "NES",
    p.adjust    = "FDR",
    direction   = "Direction"
  ) |>
  tab_header(title = "RNA-seq: baseline age difference — key enriched pathways")

table_rnaseq_baseline_pathways


saveRDS(table_rnaseq_baseline_genes,    "figures/tables/tab-rnaseq-baseline-genes.RDS")
saveRDS(table_rnaseq_baseline_pathways, "figures/tables/tab-rnaseq-baseline-pathways.RDS")
gtsave(table_rnaseq_baseline_genes,    "figures/tables/tab-rnaseq-baseline-genes.html")
gtsave(table_rnaseq_baseline_pathways, "figures/tables/tab-rnaseq-baseline-pathways.html")
