#' Create Piechart Config
#'
#' @param label_attribute A stirng
create_piechart_config <- function(label_attribute) {
  config <- list("label_attribute" = label_attribute)
  return(config)
}


#' Create Piechart
#'
#' @param data A data frame
#' @param config A named list. All values must be columns in data:
#'  - "label_attribute" (required)
#' @param palette A name of a viridis() palette
#'
#' @importFrom rlang .data
create_piechart <- function(data, config, palette = "viridis") {
  data %>%
    dplyr::select("labels" = config$label_attribute) %>%
    tidyr::drop_na() %>%
    dplyr::count(.data$labels, name = "values") %>%
    create_plotly_piechart(palette = palette)
}

#' Create Plotly Piechart
#'
#' @param plot_data A dataframe
#' @param labels_col A name of the column
#' @param values_col A name of a column
#' @param palette A name of a viridis() palette
#'
#' @importFrom magrittr %>%
create_plotly_piechart <- function(
    plot_data,
    labels_col = "labels",
    values_col = "values",
    palette = "viridis"
) {

  select_cols <- c(
    "labels"     = labels_col,
    "values"     = values_col
  )

  plot_data <- dplyr::select(plot_data, dplyr::all_of(select_cols))
  colors <- get_viridis_colors_from_tbl(plot_data, palette, "labels")

  p <- plotly::plot_ly(
    plot_data,
    labels = ~labels,
    values = ~values,
    marker = list("colors" = colors),
    type = "pie"
  )
  return(p)
}
