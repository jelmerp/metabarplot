#' Color Palettes for Taxonomic Barplots
#'
#' Predefined color palettes for use with [pbar()].
#'
#' @name metabarplot-colors
NULL

#' Default 15-color Palette
#'
#' @format A character vector of hex colors.
#' @rdname metabarplot-colors
#' @export
cols1 <- c(
  "#a74bb4", "#62b54f", "#7064d3", "#b5b348", "#dd6fc5",
  "#4db18a", "#ce416e", "#45aecf", "#d55035", "#7784cb",
  "#cc8e3e", "#ac6090", "#647934", "#df837e", "#9c5736"
)

#' BrewerPlus-inspired Distinct Palette
#'
#' Originally based on `microViz::distinct_palette(pal = "brewerPlus", add = NA)`.
#'
#' @format A character vector of hex colors.
#' @rdname metabarplot-colors
#' @export
cols_brewerplus <- c(
  "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
  "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
  "#FFFF99", "#B15928", "#1ff8ff", "#1B9E77", "#D95F02",
  "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D",
  "#666666", "#4b6a53", "#b249d5", "#7edc45", "#5c47b8",
  "#cfd251", "#ff69b4", "#69c86c", "#cd3e50", "#83d5af",
  "#da6130", "#5e79b2", "#c29545", "#532a5a", "#5f7b35",
  "#c497cf", "#773a27", "#7cb9cb", "#594e50", "#d3c4a8",
  "#c17e7f"
)

#' Kelly-inspired Distinct Palette
#'
#' Based on `microViz::distinct_palette(pal = "kelly", add = NA)` with gray
#' colors removed.
#'
#' @format A character vector of hex colors.
#' @rdname metabarplot-colors
#' @export
cols_kelly <- c(
  "#f3c300", "#875692", "#f38400", "#a1caf1", "#be0032",
  "#c2b280", "#008856", "#e68fac", "#0067a5", "#f99379",
  "#604e97", "#f6a600", "#b3446c", "#dcd300", "#882d17",
  "#8db600", "#654522", "#e25822"
)
