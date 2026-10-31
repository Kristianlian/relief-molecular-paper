# ============================================================
# plot-helpers.R  (revised for compact, publication-size figures)
# Shared plotting functions for RNA-seq figures.
#
# Changes vs. the first version:
#  - base_size = 8 (text sized for a 16 cm wide figure)
#  - volcano: y-axis label is -log2(FDR) (it plots -log2(FDR)), smaller points,
#    labels limited to the top `label_n` genes, unnamed ENSG IDs shortened,
#    stronger label repulsion, extra room on the x-axis, two-line x-labels allowed
#  - gse_plot: shorter y-axis title for the lower panel (the two titles collided),
#    compact legend, panels aligned left/right
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# parse_target()
# Splits a seqwrap `target` column (e.g. "ENSG00000000003.16_TSPAN6")
# into ensembl / version / symbol columns.
# ------------------------------------------------------------
parse_target <- function(data) {
  data |>
    tidyr::separate_wider_regex(
      target,
      patterns = c(
        ensembl  = "ENSG\\d+",
        "\\.",
        version  = "[^_]+",
        "_",
        symbol   = ".+"
      )
    )
}

# ------------------------------------------------------------
# volcano_plot()
# One volcano panel (estimate vs. -log2(FDR)) for one contrast term.
#
# summaries    - seqwrap_summarise() summaries data frame
# term_name    - contrast term (e.g. "ageyoung", "young_old_t3")
# label_thresh - only genes above this -log2(FDR) can be labelled
#                (Inf = no labels)
# label_n      - at most this many genes are labelled (the most significant)
# y_max        - upper y-axis limit
# x_label      - x-axis label; use <br> for a line break
# title        - optional panel title
# base_size    - base font size (pt); 8 suits a 16 cm wide figure
# label_size   - gene label size in mm (2.3 mm is about 6.5 pt)
# ------------------------------------------------------------
volcano_plot <- function(summaries,
                         term_name,
                         label_thresh = 40,
                         label_n = 12,
                         y_max = 85,
                         x_label = "Estimate (log<sub>2</sub> difference)",
                         title = NULL,
                         base_size = 8,
                         label_size = 2.3) {

  dat <- summaries |>
    dplyr::filter(term == term_name) |>
    parse_target() |>
    dplyr::mutate(
      log2estimate = estimate / log(2),
      p.value      = p.adjust(p.value, method = "BH"),
      neglog2p     = -log2(p.value),
      # Shorten unnamed genes, e.g. ENSG00000301137 -> ENSG…301137
      label        = dplyr::if_else(grepl("^ENSG", symbol),
                                    sub("^ENSG0*", "ENSG…", symbol),
                                    symbol),
      rank_sig     = rank(-neglog2p, ties.method = "first"),
      flag         = dplyr::if_else(neglog2p > label_thresh & rank_sig <= label_n,
                                    label, ""))

  p <- dat |>
    ggplot(aes(log2estimate, neglog2p)) +
    geom_hline(yintercept = -log2(0.05), lty = 2, color = "gray60", linewidth = 0.3) +
    annotate("text", x = min(dat$log2estimate, na.rm = TRUE),
             y = -log2(0.05) + 0.02 * y_max,
             label = "FDR 5%", size = label_size, hjust = 0, vjust = 0,
             color = "gray50") +
    geom_point(alpha = 0.3, color = "#0173B2", size = 0.8, stroke = 0) +
    ggrepel::geom_text_repel(aes(label = flag),
                             size = label_size,
                             max.overlaps = Inf,
                             min.segment.length = 0,
                             box.padding = 0.35,
                             point.padding = 0.25,
                             segment.size = 0.25,
                             segment.color = "gray30",
                             seed = 1) +
    scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
    scale_x_continuous(expand = expansion(mult = 0.12)) +
    labs(x = x_label, y = "−log<sub>2</sub>(FDR)", title = title) +
    theme_classic(base_size = base_size) +
    theme(axis.title.x = ggtext::element_markdown(lineheight = 1.1),
          axis.title.y = ggtext::element_markdown(),
          plot.title   = element_text(size = base_size + 1, face = "bold"))

  return(p)
}

# ------------------------------------------------------------
# gse_plot()
# Running enrichment-score lines + ranked contrast statistics for a few
# GO gene sets from a gseGO() result object.
#
# NOTE: colscale must have length(genesets) + 1 values
# (1 background colour + 1 per gene set).
# ------------------------------------------------------------
gse_plot <- function(gseresults,
                     genesets = c("GO:0042775", "GO:0000398"),
                     colscale = c("gray80", "#DE8F05", "#009E73"),
                     base_size = 8) {

  # Get the running score
  pdat <- list()
  for(i in seq_along(genesets)) {
    temp_dat <- enrichplot::gseaplot(gseresults, geneSetID = genesets[i])
    pdat[[i]] <- temp_dat[[1]]$data
  }

  # Combine data set
  dat <- bind_rows(pdat) |>
    mutate(Description = stringr::str_to_sentence(Description),
           Description = gsub("mrna", "mRNA", Description),
           Description = gsub("^Rna ", "RNA ", Description),   # "Rna splicing" -> "RNA splicing"
           Description = fct_reorder(Description, runningScore,
                                     .fun = max))

  p1 <- dat |>
    ggplot(aes(x, runningScore)) +
    geom_line(aes(color = Description), linewidth = 0.5) +
    scale_color_manual(values = colscale[-1]) +
    theme_classic(base_size = base_size) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = base_size),
          legend.key.size = unit(3, "mm"),
          legend.margin = margin(0, 0, 0, 0),
          legend.position  = "top",
          legend.direction = "vertical",
          legend.justification = "left") +
    labs(y = "Enrichment score") +
    guides(color = guide_legend(reverse = TRUE))

  p2 <- dat |>
    ggplot(aes(x = x, xend = x,
               y = 0, yend = geneList)) +
    geom_segment(color = colscale[1], linewidth = 0.2) +
    geom_segment(data = filter(dat, position == 1),
                 aes(color = Description), linewidth = 0.3) +
    theme_classic(base_size = base_size) +
    scale_color_manual(values = colscale[-1]) +
    theme(legend.position = "none") +
    # Short title: the long two-line title collided with the upper panel's title
    labs(y = "Contrast\nz-score",
         x = "Gene rank position")

  p <- cowplot::plot_grid(p1, p2, align = "v", axis = "lr",
                          ncol = 1,
                          rel_heights = c(1, 0.6))

  return(p)
}

# ------------------------------------------------------------
# sig_count()
# Counts genes below a given FDR threshold for one contrast term.
# Used for inline values in the results text.
# ------------------------------------------------------------
sig_count <- function(term_name, data, fdr = 0.05) {
  data |>
    dplyr::filter(term == term_name) |>
    dplyr::mutate(p.adj = p.adjust(p.value, method = "BH")) |>
    dplyr::filter(p.adj < fdr) |>
    nrow()
}
