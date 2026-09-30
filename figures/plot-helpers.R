# ============================================================
# plot-helpers.R
# Shared plotting functions for RNA-seq figures.
# Sourced by both the main analysis qmd and any standalone
# figure scripts, so there is a single source of truth.
# ============================================================

library(tidyverse)

# ------------------------------------------------------------
# parse_target()
# Splits a seqwrap `target` column (e.g. "ENSG00000000003.16_TSPAN6")
# into ensembl / version / symbol columns. Used by volcano plots
# and GSEA gene-list construction throughout the RNA-seq analysis.
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
# Builds a single volcano panel (estimate vs. -log2(FDR)) for one
# contrast term from an m2s$summaries-style data frame. Mirrors
# the p1/p1_young/p1_old chunks used in the main qmd.
#
# summaries   - the seqwrap_summarise() summaries data frame
# term_name   - contrast term to filter to (e.g. "ageyoung", "old_t3")
# label_thresh- -log2(FDR) cutoff above which genes get a text label
# y_max       - upper y-axis limit
# x_label     - x-axis label (varies by contrast, e.g. baseline vs. training)
# title       - panel title (e.g. "Young", "Old")
# ------------------------------------------------------------
volcano_plot <- function(summaries, 
                         term_name, 
                         label_thresh = 40, 
                         y_max = 85, 
                         x_label = "Estimate (log<sub>2</sub> difference)",
                         title = NULL) {
  
  dat <- summaries |> 
    dplyr::filter(term == term_name) |> 
    parse_target() |> 
    dplyr::mutate(log2estimate = estimate / log(2), 
                  p.value = p.adjust(p.value, method = "BH"), 
                  flag = dplyr::if_else(-log2(p.value) > label_thresh, symbol, ""))
  
  p <- dat |> 
    ggplot(aes(log2estimate, -log2(p.value))) + 
    geom_hline(yintercept = -log2(0.05), lty = 2, color = "gray") +
    annotate("text", x = min(dat$log2estimate, na.rm = TRUE), 
             y = -log2(0.05) + 1.5, 
             label = "FDR 5%", size = 3, hjust = 0, color = "gray") +
    geom_point(alpha = 0.3, color = "#0173B2") + 
    ggrepel::geom_text_repel(aes(label = flag), size = 3, 
                             max.overlaps = Inf, min.segment.length = 0) +
    theme_classic() + 
    labs(x = x_label, y = "log<sub>2</sub>(FDR)", title = title) + 
    theme(axis.title.x = ggtext::element_markdown(),
          axis.title.y = ggtext::element_markdown()) + 
    scale_y_continuous(limits = c(0, y_max), expand = c(0,0))
  
  return(p)
}

# ------------------------------------------------------------
# gse_plot()
# Plots enrichment-score running lines + effect-size rank bars 
# for a small set of GO gene sets from a gseGO() result object.
#
# NOTE: colscale must have length(genesets) + 1 values 
# (1 background color + 1 per geneset).
# ------------------------------------------------------------
gse_plot <- function(gseresults, genesets = c("GO:0042775", "GO:0000398"), 
                     colscale = c("gray80", "#DE8F05", "#009E73")) {
  
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
           Description = fct_reorder(Description, runningScore, 
                                     .fun = max))
  
  p1 <- dat |> 
    ggplot(aes(x, runningScore)) + 
    geom_line(aes(color = Description)) +
    scale_color_manual(values = colscale[-1]) +
    theme_classic() + 
    theme(axis.text.x = element_blank(), 
          axis.title.x = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.line.x = element_blank(), 
          legend.title = element_blank(), 
          legend.position  = "top",
          legend.direction = "vertical", 
          legend.justification = "left") +
    labs(y = "Enrichment score") + 
    guides(color = guide_legend(reverse = TRUE))
  
  p2 <-  dat |> 
    ggplot(aes(x = x, xend = x, 
               y = 0, yend = geneList)) + 
    geom_segment(color = colscale[1]) + 
    geom_segment(data = filter(dat, 
                               position == 1), 
                 aes(color = Description))  +
    theme_classic() + 
    scale_color_manual(values = colscale[-1])  + 
    theme(legend.position = "none") + 
    labs(y = "Contrast effect size\n(z-score)", 
         x = "Gene rank position")
  
  p <- cowplot::plot_grid(p1, p2, align = "v", 
                          ncol = 1, 
                          rel_heights = c(1, 0.6))
  
  return(p) 
}

# ------------------------------------------------------------
# sig_count()
# Counts genes below a given FDR threshold for one contrast term.
# Used for dynamic inline values in the results text 
# (n_diff_expressed, n_sig_young_t3, etc.)
# ------------------------------------------------------------
sig_count <- function(term_name, data, fdr = 0.05) {
  data |> 
    dplyr::filter(term == term_name) |> 
    dplyr::mutate(p.adj = p.adjust(p.value, method = "BH")) |> 
    dplyr::filter(p.adj < fdr) |> 
    nrow()
}