# =============================================================================
# Outcome summary tables matching the results text (flextable, for Word output)
#
#   make_outcome_table()   one outcome per table
#   make_combined_table()  several outcomes stacked in one table
#
# Rows: within-group change (pooled, low, mod, control / pooled, low, mod)
#       and the five template contrasts.
# Estimate [95% CI] in %, absolute change for within-group rows,
# probability in the direction of the estimate (P(>0) / P(<0)).
# One decimal, or two where one decimal would show 0.0.
#
# Works on the standard contrast objects (contrast_mv, contrast_mt,
# contrast_ublm, contrast_1rm, contrast_bc).
#
# Shared helpers fp(), fmt_prob(), add_group_rows() and style_ft() are also
# used by the peak torque table script.
# =============================================================================

library(tidyverse)
library(flextable)
library(officer)

# -----------------------------------------------------------------------------
# 1. Shared formatting helpers
# -----------------------------------------------------------------------------

# `digits` decimals, one more where that would show zero; typographic minus
fp <- function(x, digits = 1) {
  d <- ifelse(!is.na(x) & round(x, digits) == 0 & x != 0, digits + 1, digits)
  s <- mapply(function(v, k) formatC(round(v, k), format = "f", digits = k, big.mark = ","), x, d)
  gsub("-", "−", s)
}

# Probability in the direction of the estimate
fmt_prob <- function(est, pos) {
  up    <- est >= 0
  p_dir <- if_else(up, pos, 1 - pos)
  lab   <- if_else(up, "P(>0)", "P(<0)")
  
  qual <- case_when(
    p_dir >= 0.975 ~ "credible",
    p_dir >= 0.90  ~ "likely",
    p_dir >= 0.75  ~ "weak",
    TRUE           ~ "no"
  )
  
  pct_str <- if_else(p_dir >= 0.9995,
                     "> 99.9%",
                     paste0("= ", formatC(100 * p_dir, format = "f", digits = 1), "%"))
  
  paste0(lab, " ", pct_str, " (", qual, ")")
}

# Insert a header row before each group (flextable has no row groups as such).
# Returns all-character data plus a logical column .grp marking header rows.
add_group_rows <- function(d, group_col, first_col) {
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  d[] <- lapply(d, as.character)
  groups <- unique(d[[group_col]])
  keep   <- setdiff(names(d), group_col)
  out <- do.call(rbind, lapply(groups, function(g) {
    rows <- d[d[[group_col]] == g, keep, drop = FALSE]
    hdr  <- rows[1, , drop = FALSE]
    hdr[1, ] <- ""
    hdr[[first_col]] <- g
    rbind(cbind(hdr, .grp = TRUE), cbind(rows, .grp = FALSE))
  }))
  rownames(out) <- NULL
  out
}

# House style: no vertical lines, light grey row lines, darker grey top/bottom
# rules, bold group header rows spanning the table, full page width.
style_ft <- function(ft, grp, left_cols = 1) {
  thin  <- fp_border(color = "#D3D3D3", width = 0.75)
  thick <- fp_border(color = "#A8A8A8", width = 1.5)
  gi    <- which(grp)
  n     <- length(ft$col_keys)
  
  ft <- ft |>
    border_remove() |>
    fontsize(size = 9, part = "all") |>
    padding(padding.top = 2, padding.bottom = 2, part = "all") |>
    align(align = "center", part = "all") |>
    align(j = left_cols, align = "left", part = "all") |>
    border_inner_h(border = thin, part = "body") |>
    hline_top(border = thick, part = "header") |>
    hline_bottom(border = thick, part = "header") |>
    hline_bottom(border = thick, part = "body")
  
  if (length(gi)) {
    ft <- ft |>
      merge_h_range(i = gi, j1 = 1, j2 = n, part = "body") |>
      align(i = gi, j = 1, align = "left", part = "body") |>
      bold(i = gi, part = "body")
  }
  
  set_table_properties(ft, width = 1, layout = "autofit")
}

# -----------------------------------------------------------------------------
# 2. Build the formatted rows for one outcome
# -----------------------------------------------------------------------------
build_outcome_data <- function(pred, contrast,
                               timepoints      = NULL,       # e.g. c(mid = "Pre-Mid", post = "Pre-Post")
                               single_label    = "Pre-Post", # label when there is one timepoint
                               abs_digits      = 1,          # decimals for absolute change
                               baseline_digits = 1)          # decimals for baseline
{
  groups <- c(
    int_old_pooled = "Old, Intervention (pooled)",
    int_old_low    = "Old, Low",
    int_old_mod    = "Old, Mod",
    con_old_con    = "Old, Control",
    int_yng_pooled = "Young, Intervention (pooled)",
    int_yng_low    = "Young, Low",
    int_yng_mod    = "Young, Mod"
  )
  contrasts <- c(
    old_vs_con_delta = "Old: Intervention vs. Control",
    yng_vs_old_delta = "Young vs. Old (intervention)",
    yng_mod_vs_low   = "Young: Mod vs. Low",
    old_mod_vs_low   = "Old: Mod vs. Low",
    yng_vs_old_vol   = "Volume effect: Young vs. Old"
  )
  
  tp_keys <- if (is.null(timepoints)) "" else paste0("_", names(timepoints))
  tp_labs <- if (is.null(timepoints)) single_label else unname(timepoints)
  
  lookup <- bind_rows(
    expand_grid(key = names(groups), i = seq_along(tp_keys)) |>
      mutate(hypothesis = paste0(key, "_delta", tp_keys[i]),
             label = groups[key], section = "Within-group change"),
    expand_grid(key = names(contrasts), i = seq_along(tp_keys)) |>
      mutate(hypothesis = paste0(key, tp_keys[i]),
             label = contrasts[key], section = "Between-group contrasts")
  ) |>
    mutate(timepoint = tp_labs[i])
  
  # Baselines; pooled = average of the two volume conditions
  base <- pred |>
    filter(time == "pre") |>
    summarise(.by = tx, baseline = mean(m))
  pool_base <- function(a, b) mean(base$baseline[base$tx %in% c(a, b)])
  base <- bind_rows(
    base,
    tibble(tx = "int_old_pooled", baseline = pool_base("int_old_low", "int_old_mod")),
    tibble(tx = "int_yng_pooled", baseline = pool_base("int_yng_low", "int_yng_mod"))
  )
  
  lookup |>
    inner_join(select(contrast, -any_of("timepoint")), by = "hypothesis") |>
    mutate(tx = sub("_delta.*$", "", hypothesis)) |>
    left_join(base, by = "tx") |>
    mutate(
      baseline = if_else(section == "Within-group change", baseline, NA_real_),
      abs = if_else(
        is.na(baseline), NA_character_,
        sprintf("%s [%s, %s]",
                fp((mean - 1) * baseline,    abs_digits),
                fp((lower95 - 1) * baseline, abs_digits),
                fp((upper95 - 1) * baseline, abs_digits))
      ),
      baseline = if_else(is.na(baseline), NA_character_,
                         formatC(baseline, format = "f", digits = baseline_digits, big.mark = ",")),
      pct  = sprintf("%s%% [%s%%, %s%%]", fp(pct_change), fp(pct_lower), fp(pct_upper)),
      prob = fmt_prob(pct_change, pos)
    ) |>
    select(section, label, timepoint, baseline, abs, pct, prob)
}

# -----------------------------------------------------------------------------
# 3. Render rows as a flextable
# -----------------------------------------------------------------------------
# title = NULL and source_note = FALSE (defaults): no caption or note inside
# the table, for use with a Quarto tbl-cap in the manuscript.
render_outcome_table <- function(dat, title = NULL,
                                 baseline_label, abs_label,
                                 show_time = TRUE,
                                 center_label = "means",
                                 source_note = FALSE) {
  d <- dat |>
    mutate(
      baseline  = coalesce(baseline, "—"),
      abs       = coalesce(abs, "—"),
      label     = as.character(label),
      timepoint = as.character(timepoint)
    )
  if (!show_time) d <- select(d, -timepoint)
  
  d   <- add_group_rows(d, group_col = "section", first_col = "label")
  grp <- d$.grp
  d$.grp <- NULL
  
  labels <- list(
    label     = "Group / Contrast",
    timepoint = "Timepoint",
    baseline  = baseline_label,
    abs       = abs_label,
    pct       = "Δ (%, 95% CI)",
    prob      = "Probability"
  )
  
  ft <- flextable(d) |>
    set_header_labels(values = labels[names(d)]) |>
    style_ft(grp, left_cols = 1)
  
  if (!is.null(title)) ft <- set_caption(ft, title)
  
  if (source_note) {
    ft <- add_footer_lines(ft, paste0(
      "Values are posterior ", center_label, " with 95% equal-tailed credible intervals (CI). ",
      "Percent values are (ratio − 1) × 100, so 0% indicates no change or no difference; ",
      "between-group contrasts are ratios of within-group ratios. ",
      "Pooled values are the equal-weighted geometric mean of the volume conditions. ",
      "Probability is the posterior probability in the direction of the estimate, qualified as: ",
      "credible when the 95% CI excluded zero (P ≥ 97.5%); likely when P ≥ 90%; ",
      "weak evidence when 75% ≤ P < 90%; no evidence when P < 75%. ",
      "Absolute changes are derived from the percent change and the group's baseline."
    )) |>
      fontsize(size = 8, part = "footer")
  }
  
  ft
}

# -----------------------------------------------------------------------------
# 4a. One outcome per table
# -----------------------------------------------------------------------------
# make_outcome_table(pred_mt, contrast_mt, unit = "mm",
#                    timepoints = c(mid = "Pre-Mid", post = "Pre-Post"))
make_outcome_table <- function(pred, contrast, unit, title = NULL,
                               timepoints = NULL,
                               abs_digits = 1,
                               baseline_digits = 1,
                               center_label = "means",
                               source_note = FALSE) {
  build_outcome_data(pred, contrast, timepoints,
                     abs_digits = abs_digits, baseline_digits = baseline_digits) |>
    render_outcome_table(
      title           = title,
      baseline_label  = paste0("Baseline (", unit, ")"),
      abs_label       = paste0("Δ (", unit, ", 95% CI)"),
      show_time       = !is.null(timepoints),
      center_label    = center_label,
      source_note     = source_note
    )
}

# -----------------------------------------------------------------------------
# 4b. Several outcomes in one table
# -----------------------------------------------------------------------------
# outcomes: named list, one element per outcome (name = block title), each
#   list(pred = , contrast = , unit = , timepoints = , single_label = ,
#        abs_digits = , baseline_digits = )   # digits default to 1
make_combined_table <- function(outcomes, title = NULL,
                                center_label = "means",
                                source_note = FALSE) {
  dat <- purrr::imap_dfr(outcomes, function(o, nm) {
    build_outcome_data(o$pred, o$contrast,
                       timepoints      = o$timepoints,
                       single_label    = o$single_label    %||% "Pre-Post",
                       abs_digits      = o$abs_digits      %||% 1,
                       baseline_digits = o$baseline_digits %||% 1) |>
      mutate(section = paste0(nm, " (", o$unit, ") — ", section))
  })
  
  render_outcome_table(
    dat,
    title           = title,
    baseline_label  = "Baseline",
    abs_label       = "Δ (absolute, 95% CI)",
    show_time       = TRUE,
    center_label    = center_label,
    source_note     = source_note
  )
}