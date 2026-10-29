# ==========================================================
#   Protein and total RNA: one combined table
#   NOT RUN. Object and file names are placeholders (TODO).
# ==========================================================
# Needs the contrast objects to contain the pooled rows, i.e. hypotheses such as
#   int_old_pooled_delta_w3, int_old_pooled_delta_post, int_yng_pooled_delta_w3, ...
# (protein, one timepoint: int_old_pooled_delta, int_yng_pooled_delta).
# Without them, make_outcome_table() silently drops the pooled rows (inner_join).

source("./figures/tables/make_outcome_table.R")

# TODO: your saved objects
pred_rna     <- readRDS("data/data-gen/pred_rna.rds")
contrast_rna <- readRDS("data/data-gen/contrast_rna.rds")
pred_ubf     <- readRDS("data/data-gen/pred_ubf.rds")
contrast_ubf <- readRDS("data/data-gen/contrast_ubf.rds")
pred_cmyc     <- readRDS("data/data-gen/pred_cmyc_high.rds")
contrast_cmyc <- readRDS("data/data-gen/contrast_cmyc_high.rds")

contrast_rna |> count(hypothesis, delta)     # which rows exist? (16 in total)

adapt_contrast <- function(x) {
  x |>
    mutate(tp = case_when(delta == "pre to w3"   ~ "w3",
                          delta == "pre to post" ~ "post"),
           hypothesis = paste0(hypothesis, "_", tp),
           pct_change = (mean - 1) * 100,
           pct_lower  = (lower95 - 1) * 100,
           pct_upper  = (upper95 - 1) * 100) |>
    select(-delta, -tp)
}
contrast_rna <- adapt_contrast(contrast_rna)

table_rna_protein <- make_combined_table(
  outcomes = list(
    "Total RNA" = list(
      pred = pred_rna, contrast = contrast_rna, unit = "ng/mg",
      timepoints = c(w3 = "Pre-W3", post = "Pre-Post"),
      abs_digits = 0, baseline_digits = 0),
    "UBF" = list(
      pred = pred_ubf, contrast = contrast_ubf, unit = "corrected area",   # TODO unit
      timepoints = NULL, single_label = "Pre-W3",
      abs_digits = 0, baseline_digits = 0),
    "c-Myc, high peak" = list(
      pred = pred_cmyc, contrast = contrast_cmyc, unit = "corrected area",
      timepoints = NULL, single_label = "Pre-W3",
      abs_digits = 0, baseline_digits = 0)
  ),
  title = NULL,            # use a Quarto tbl-cap instead
  source_note = FALSE
)

saveRDS(table_rna_protein, "figures/tables/tab-rna-protein.rds")
save_as_html(table_rna_protein, path = "figures/tables/tab-rna-protein.html")
