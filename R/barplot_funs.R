#' Create Taxonomic Abundance Barplots
#'
#' Build a stacked abundance barplot from either a `phyloseq` object or a
#' precomputed abundance table.
#'
#' @param ps A `phyloseq` object. Required when `abund_df` is `NULL`.
#' @param taxrank Taxonomic rank to summarize abundance by.
#' @param axis_var Variable to plot on the y-axis. Use `"Sample"` for
#'   per-sample bars, or a metadata column name.
#' @param facet_var Optional metadata column for faceting rows.
#' @param facet_var2 Optional second metadata column for faceting columns.
#' @param axis_lab Axis title for the y-axis.
#' @param abund_tres Taxa with mean abundance below this threshold are lumped
#'   into `rare_label`. Set to `NA` to disable threshold-based lumping.
#' @param focal_taxa Optional vector of taxa to keep. Non-focal taxa are lumped
#'   into `rare_label`.
#' @param na_to_unknown If `TRUE`, missing taxon labels are converted to
#'   `unknown_label`.
#' @param sort_by_abund If `TRUE`, taxa are ordered by mean abundance.
#' @param colors Optional vector of colors. If `NULL`, a distinct palette is
#'   generated.
#' @param abund_df Optional abundance table, typically produced by
#'   [abund_stats()]. Must include `Abundance`, `Sample`, and `taxrank` columns.
#' @param convert_abund If `TRUE`, convert per-sample counts in `ps` to relative
#'   abundances.
#' @param return_abund If `TRUE`, return the abundance table and skip plotting.
#' @param unknown_label Label to use for unknown taxa.
#' @param rare_label Label to use for lumped low-abundance taxa.
#' @param alpha Opacity of fill colors.
#' @param facet_scales Facet scale setting passed to `ggh4x::facet_grid2()`.
#'
#' @return A `ggplot2` object by default, or a tibble if `return_abund = TRUE`.
#' @export
#'
#' @examples
#' abund_df <- tibble::tibble(
#'   Sample = c("S1", "S1", "S2", "S2"),
#'   Phylum = c("Firmicutes", "Proteobacteria", "Firmicutes", "Proteobacteria"),
#'   Abundance = c(0.4, 0.6, 0.7, 0.3)
#' )
#' p <- pbar(abund_df = abund_df, taxrank = "Phylum")
#' p
pbar <- function(
    ps = NULL,
    taxrank = "Phylum",
    axis_var = "Sample",
    facet_var = NULL,
    facet_var2 = NULL,
    axis_lab = NULL,
    abund_tres = 0.01,
    focal_taxa = NULL,
    na_to_unknown = TRUE,
    sort_by_abund = TRUE,
    colors = cols_kelly,
    abund_df = NULL,
    convert_abund = FALSE,
    return_abund = FALSE,
    unknown_label = "unknown",
    rare_label = "other (rare)",
    alpha = 1,
    facet_scales = "free") {

  if (is.null(abund_df)) {
    if (is.null(ps)) {
      stop("Provide either `ps` or `abund_df`.", call. = FALSE)
    }
  }

  if (convert_abund) {
    ps <- phyloseq::transform_sample_counts(ps, function(x) x / sum(x))
  }

  if (is.null(abund_df)) {
    abund_df <- abund_stats(
      ps = ps,
      taxrank = taxrank,
      abund_tres = abund_tres,
      focal_taxa = focal_taxa,
      na_to_unknown = na_to_unknown,
      sort_by_abund = sort_by_abund,
      groupby = c(axis_var, facet_var, facet_var2),
      unknown_label = unknown_label,
      rare_label = rare_label
    )
    if (return_abund) {
      return(abund_df)
    }
  }

  required_cols <- c("Abundance", axis_var, taxrank)
  missing_cols <- setdiff(required_cols, colnames(abund_df))
  if (length(missing_cols) > 0) {
    stop(
      "`abund_df` is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  ntax <- length(unique(stats::na.omit(abund_df[[taxrank]])))
  if (is.null(colors)) {
    if (requireNamespace("randomcoloR", quietly = TRUE)) {
      colors <- randomcoloR::distinctColorPalette(k = ntax)
    } else {
      colors <- grDevices::hcl.colors(ntax, palette = "Dynamic")
    }
  } else {
    if (length(colors) < ntax) {
      stop("`colors` must have at least as many values as the number of taxa.", call. = FALSE)
    }
    colors <- colors[seq_len(ntax)]
  }

  if (any(abund_df[[taxrank]] == rare_label, na.rm = TRUE)) {
    if (any(abund_df[[taxrank]] == unknown_label, na.rm = TRUE)) {
      colors[length(colors) - 1] <- "grey65"
      colors[length(colors)] <- "grey85"
    } else {
      colors[length(colors)] <- "grey65"
    }
  } else if (any(abund_df[[taxrank]] == unknown_label, na.rm = TRUE)) {
    colors[length(colors)] <- "grey85"
  }

  legend_element <- ggtext::element_markdown()

  p <- ggplot2::ggplot(abund_df) +
    ggplot2::aes(
      x = .data$Abundance,
      y = .data[[axis_var]],
      fill = .data[[taxrank]]
    ) +
    ggplot2::geom_col(
      color = "grey20",
      alpha = alpha,
      position = ggplot2::position_stack(reverse = TRUE)
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.003)),
      labels = scales::label_percent()
    ) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::scale_fill_manual(
      values = colors,
      guide = ggplot2::guide_legend(ncol = 1, reverse = FALSE)
    ) +
    ggplot2::labs(y = axis_lab) +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.text = legend_element
    )

  if (!is.null(facet_var)) {
    if (is.null(facet_var2)) {
      p <- p +
        ggh4x::facet_grid2(
          rows = ggplot2::vars(.data[[facet_var]]),
          scales = facet_scales,
          space = "free_y",
          axes = "margins"
        )
    } else {
      p <- p +
        ggh4x::facet_grid2(
          rows = ggplot2::vars(.data[[facet_var]]),
          cols = ggplot2::vars(.data[[facet_var2]]),
          scales = facet_scales,
          space = "fixed",
          axes = "margins",
          independent = "y"
        )
    }
  }

  p
}

#' Summarize Per-Taxon Abundances
#'
#' Aggregate abundances by taxonomic rank for plotting.
#'
#' @param ps A `phyloseq` object.
#' @param taxrank Taxonomic rank column to summarize.
#' @param groupby Optional grouping variables. If `NULL`, per-sample abundances
#'   are kept.
#' @param abund_tres Mean abundance threshold below which taxa are lumped into
#'   `rare_label`. Set to `NA` to disable threshold-based lumping.
#' @param focal_taxa Optional vector of taxa to retain. If provided,
#'   `abund_tres` is ignored.
#' @param na_to_unknown If `TRUE`, missing taxon labels are set to
#'   `unknown_label`.
#' @param sort_by_abund If `TRUE`, taxa are ordered by mean abundance.
#' @param unknown_label Label to use for unknown taxa.
#' @param rare_label Label to use for lumped taxa.
#'
#' @return A tibble with abundance summaries.
#' @export
abund_stats <- function(
    ps,
    taxrank,
    groupby = NULL,
    abund_tres = 0.01,
    focal_taxa = NULL,
    na_to_unknown = TRUE,
    sort_by_abund = TRUE,
    unknown_label = "unknown",
    rare_label = "other (rare)") {
  if (!is.null(focal_taxa)) {
    abund_tres <- NA
    focal_taxa <- c(focal_taxa, unknown_label)
  }

  groupby <- groupby[!is.na(groupby)]
  if (length(groupby) == 0) {
    groupby <- NULL
  }

  ps <- phyloseq::tax_glom(ps, taxrank = taxrank, NArm = FALSE)

  df <- phyloseq::psmelt(ps)
  meta <- methods::as(phyloseq::sample_data(ps), "data.frame")
  meta <- tibble::rownames_to_column(meta, "Sample")

  if (any(is.na(df[[taxrank]]))) {
    na_df <- df |>
      dplyr::filter(is.na(.data[[taxrank]])) |>
      dplyr::group_by(.data$Sample) |>
      dplyr::summarize(Abundance = sum(.data$Abundance), .groups = "drop") |>
      dplyr::left_join(meta, by = "Sample")

    na_df[[taxrank]] <- NA
    na_df$OTU <- "unknown"

    df <- df |>
      dplyr::filter(!is.na(.data[[taxrank]])) |>
      dplyr::bind_rows(na_df)
  }

  if (na_to_unknown) {
    df[[taxrank]][is.na(df[[taxrank]])] <- unknown_label
  }

  df <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(taxrank, "Sample")))) |>
    dplyr::summarize(Abundance = sum(.data$Abundance), .groups = "drop")

  df <- df |>
    dplyr::left_join(meta, by = "Sample") |>
    dplyr::select(!dplyr::contains(".y"))
  colnames(df) <- sub("\\.x$", "", colnames(df))

  if (!is.null(groupby)) {
    df <- df |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(groupby, taxrank)))) |>
      dplyr::summarize(Abundance = mean(.data$Abundance), .groups = "drop")

    if (groupby[1] == "Sample") {
      df <- df |>
        dplyr::left_join(meta, by = "Sample") |>
        dplyr::select(!dplyr::contains(".y"))
      colnames(df) <- sub("\\.x$", "", colnames(df))
    }
  }

  to_lump <- character(0)
  if (!is.na(abund_tres)) {
    to_lump <- df |>
      dplyr::group_by(.data[[taxrank]]) |>
      dplyr::summarize(mean_abund = mean(.data$Abundance), .groups = "drop") |>
      dplyr::filter(.data$mean_abund <= abund_tres) |>
      dplyr::pull(.data[[taxrank]])

    message(length(to_lump), " low-abundance taxa will be lumped into a new category 'other'")
  } else if (!is.null(focal_taxa)) {
    to_lump <- df |>
      dplyr::filter(!.data[[taxrank]] %in% focal_taxa) |>
      dplyr::pull(.data[[taxrank]])

    message(length(to_lump), " non-focal taxa will be lumped into a new category 'other'")
  }

  if (!is.na(abund_tres) || !is.null(focal_taxa)) {
    grouping_cols <- if (is.null(groupby)) "Sample" else groupby

    other_df <- df |>
      dplyr::filter(.data[[taxrank]] %in% to_lump) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
      dplyr::summarize(Abundance = sum(.data$Abundance), .groups = "drop")

    if (!is.null(groupby) && groupby[1] == "Sample") {
      other_df <- other_df |>
        dplyr::left_join(meta, by = "Sample") |>
        dplyr::select(!dplyr::contains(".y"))
      colnames(other_df) <- sub("\\.x$", "", colnames(other_df))
    }

    other_df[[taxrank]] <- rare_label

    df <- df |>
      dplyr::filter(!.data[[taxrank]] %in% to_lump) |>
      dplyr::bind_rows(other_df)

    if (sort_by_abund) {
      tax_ord <- df |>
        dplyr::group_by(.data[[taxrank]]) |>
        dplyr::summarize(abund = mean(.data$Abundance), .groups = "drop") |>
        dplyr::arrange(-.data$abund) |>
        stats::na.omit() |>
        dplyr::pull(.data[[taxrank]])

      df[[taxrank]] <- factor(df[[taxrank]], levels = tax_ord)
    }

    df[[taxrank]] <- forcats::fct_relevel(df[[taxrank]], rare_label, after = Inf)

  } else if (sort_by_abund) {
    tax_ord <- df |>
      dplyr::group_by(.data[[taxrank]]) |>
      dplyr::summarize(abund = mean(.data$Abundance), .groups = "drop") |>
      dplyr::arrange(-.data$abund) |>
      stats::na.omit() |>
      dplyr::pull(.data[[taxrank]])

    df[[taxrank]] <- factor(df[[taxrank]], levels = tax_ord)
  }

  if (na_to_unknown) {
    df[[taxrank]] <- forcats::fct_relevel(df[[taxrank]], unknown_label, after = Inf)
  }

  if (taxrank == "Genus") {
    df <- df |> dplyr::mutate(Genus = sub("([A-Z].*)", "*\\1*", .data$Genus))
  }

  if (taxrank == "genus") {
    df <- df |> dplyr::mutate(genus = sub("([A-Z].*)", "*\\1*", .data$genus))
  }

  tibble::as_tibble(df)
}

#' Build an Abundance Table from Taxonomy Data
#'
#' Create an abundance table compatible with [pbar()] from a taxonomy table,
#' optionally weighted by an abundance column.
#'
#' @param tax_df Taxonomy data frame with one column per taxonomic rank.
#' @param tax_rank Taxonomic rank column name in `tax_df`.
#' @param abund_column Optional abundance column in `tax_df`.
#' @param method_column Method/database column in `tax_df`.
#' @param method_id Sample identifier for output. Defaults to first value in
#'   `method_column`.
#' @param abund_tres Taxa at or below this abundance are lumped into
#'   `rare_label`. Set to `NA` to disable threshold-based lumping.
#' @param sort_by_abund If `TRUE`, taxa are sorted by abundance.
#' @param na_to_unknown If `TRUE`, `NA` taxa are replaced with `unknown_label`.
#' @param unknown_label Label for unknown taxa.
#' @param rare_label Label for low-abundance taxa.
#'
#' @return A tibble with columns for `tax_rank`, `Abundance`, and `Sample`.
#' @export
#'
#' @examples
#' x <- tibble::tibble(
#'   method = "demo",
#'   Genus = c("A", "B", NA),
#'   reads = c(80, 15, 5)
#' )
#' tax_stats(x, tax_rank = "Genus", abund_column = "reads")
tax_stats <- function(
    tax_df,
    tax_rank,
    abund_column = NULL,
    method_column = "method",
    method_id = NULL,
    abund_tres = 0.01,
    sort_by_abund = TRUE,
    na_to_unknown = TRUE,
    unknown_label = "unknown",
    rare_label = "other (rare)") {

  if (is.null(method_id)) {
    method_id <- tax_df[[method_column]][1]
  }

  if (is.null(abund_column)) {
    df <- tax_df |> dplyr::mutate(Abundance = 1 / nrow(tax_df))
  } else {
    df <- tax_df |>
      dplyr::mutate(
        Abundance = .data[[abund_column]] /
          sum(.data[[abund_column]], na.rm = TRUE)
      )
  }

  df <- df |>
    dplyr::summarize(
      Abundance = sum(.data$Abundance, na.rm = TRUE),
      .by = dplyr::all_of(tax_rank)
    )

  if (na_to_unknown) {
    df[[tax_rank]][is.na(df[[tax_rank]])] <- unknown_label
  } else {
    df <- df |> dplyr::filter(!is.na(.data[[tax_rank]]))
  }

  taxa_to_lump <- character(0)
  ntaxa_to_lump <- 0
  if (!is.na(abund_tres)) {
    taxa_to_lump <- df |>
      dplyr::filter(.data$Abundance <= abund_tres) |>
      dplyr::pull(.data[[tax_rank]])

    ntaxa_to_lump <- sum(!is.na(taxa_to_lump))
    message(ntaxa_to_lump, " low-abundance taxa will be lumped into 'other'")
  }

  if (ntaxa_to_lump > 0) {
    to_lump_df <- df |>
      dplyr::filter(.data[[tax_rank]] %in% taxa_to_lump) |>
      dplyr::summarize(Abundance = sum(.data$Abundance), .groups = "drop")

    to_lump_df[[tax_rank]] <- rare_label

    df <- df |>
      dplyr::filter(!.data[[tax_rank]] %in% taxa_to_lump) |>
      dplyr::bind_rows(to_lump_df)
  }

  if (sort_by_abund) {
    tax_order <- df |>
      dplyr::group_by(.data[[tax_rank]]) |>
      dplyr::summarize(abund = mean(.data$Abundance), .groups = "drop") |>
      dplyr::arrange(-.data$abund) |>
      stats::na.omit() |>
      dplyr::pull(.data[[tax_rank]])

    tax_order <- setdiff(tax_order, c(rare_label, unknown_label))
    tax_order <- c(tax_order, rare_label, unknown_label)
    df[[tax_rank]] <- factor(df[[tax_rank]], levels = unique(tax_order))
  }

  df$Sample <- method_id

  tibble::as_tibble(df)
}

#' Create a Dummy Phyloseq Object from a Taxonomy Table
#'
#' Build a one-sample `phyloseq` object where each ASV has count 1. This is
#' useful for testing and demonstration.
#'
#' @param taxtable Data frame with an `ASV` column and taxonomy columns.
#' @param tax_levels Character vector of taxonomy column names.
#'
#' @return A `phyloseq` object.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- tibble::tibble(
#'   ASV = c("ASV1", "ASV2"),
#'   Kingdom = "Bacteria",
#'   Phylum = c("Firmicutes", "Proteobacteria")
#' )
#' ps_from_taxtable(x, tax_levels = c("Kingdom", "Phylum"))
#' }
ps_from_taxtable <- function(
    taxtable,
    tax_levels = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")) {

  sample_id <- "S1"

  tax_mat <- taxtable |>
    dplyr::select(dplyr::all_of(tax_levels)) |>
    as.matrix()
  rownames(tax_mat) <- taxtable$ASV

  count_mat <- data.frame(rep(1, nrow(tax_mat)))
  colnames(count_mat) <- sample_id
  rownames(count_mat) <- rownames(tax_mat)

  meta <- data.frame(group = "S", treatment = "A")
  rownames(meta) <- sample_id

  phyloseq::phyloseq(
    phyloseq::otu_table(count_mat, taxa_are_rows = TRUE),
    phyloseq::tax_table(tax_mat),
    phyloseq::sample_data(meta)
  )
}
