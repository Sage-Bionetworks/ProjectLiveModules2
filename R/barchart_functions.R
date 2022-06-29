#' Create Barchart Config
#'
#' @param x_attribute A stirng
#' @param group_attribute A string
#' @param color_attribute A string
create_barchart_config <- function(
    x_attribute, group_attribute, color_attribute
) {
  config <- list("x_attribute" = x_attribute)

  if (group_attribute != "none") {
    config <- c(config, list("group_attribute" = group_attribute))
  }
  if (color_attribute != "none") {
    config <- c(config, list("color_attribute" = color_attribute))
  }

  return(config)
}


#' Create Barchart
#'
#' Creates a barchart based on the attributes in the config
#'
#' @param data A data frame
#' @param config A named list. All values must be columns in data:
#'  - "x_attribute"
#' @param palette A name of a viridis() palette
create_barchart <- function(data, config, palette = "viridis") {
  plot_is_grouped <- !is.null(config$group_attribute)
  plot_is_stacked <- !is.null(config$color_attribute)

  if (plot_is_grouped && plot_is_stacked) {
    plot_func <- create_stacked_grp_barchart
  } else if (plot_is_grouped) {
    plot_func <- create_grouped_barchart
  } else if (plot_is_stacked) {
    plot_func <- create_stacked_barchart
  } else {
    plot_func <- create_standard_barchart
  }

  plot_func(data, config, palette)
}

#' Create Standard Barchart
#'
#' @param data A data frame
#' @param config A named list. All values must be columns in data:
#'  - "x_attribute" (required)
#' @param palette A name of a viridis() palette
#'
#' @importFrom rlang .data
create_standard_barchart <- function(data, config, palette = "viridis") {
  data %>%
    dplyr::select("x" = config$x_attribute) %>%
    tidyr::drop_na() %>%
    dplyr::count(.data$x, name = "y") %>%
    create_plotly_barchart(text_col = "y", palette = palette)
}

#' Create Stacked Barchart
#'
#' @param data A dataframe
#' @param config A named list. All values must be columns in data:
#'  - "x_attribute"
#'  - "color_attribute"
#' @param palette A name of a viridis() palette
#'
#' @importFrom rlang .data
create_stacked_barchart <- function(data, config, palette = "viridis") {
  data %>%
    dplyr::select(
      "x" = config$x_attribute,
      "color" = config$color_attribute
    ) %>%
    tidyr::drop_na() %>%
    dplyr::count(.data$x, .data$color, name = "y") %>%
    create_plotly_barchart(
      color_col = "color",
      text_col = "y",
      palette = palette
    )
}

#' Create Grouped Barchart
#'
#' @param data A dataframe
#' @param config A named list. All values must be columns in data:
#'  - "x_attribute"
#'  - "group_attribute"
#' @param palette A name of a viridis() palette
#'
#' @importFrom rlang .data
create_grouped_barchart <- function(data, config, palette = "viridis") {
  data %>%
    dplyr::select(
      "x" = config$x_attribute,
      "group" = config$group_attribute
    ) %>%
    tidyr::drop_na() %>%
    dplyr::count(.data$x, .data$group, name = "y") %>%
    dplyr::group_by(.data$group) %>%
    dplyr::group_map(
      ~ create_plotly_barchart(
        plot_data = .,
        text_col = "y",
        xlab = .y$group,
        showlegend = FALSE,
        palette = palette
      )
    ) %>%
    plotly::subplot(nrows = 1, shareX = TRUE, shareY = TRUE) %>%
    plotly::layout(yaxis = list(title = "Count"))
}

#' Create Stacked Grouped Barchart
#'
#' @param data A dataframe
#' @param config A named list. All values must be columns in data:
#'  - "x_attribute"
#'  - "color_attribute"
#'  - "group_attribute"
#' @param palette A name of a viridis() palette
#'
#' @importFrom rlang .data
create_stacked_grp_barchart <- function(data, config, palette = "viridis") {
  data %>%
    dplyr::select(
      "x" = config$x_attribute,
      "group" = config$group_attribute,
      "color" = config$color_attribute
    ) %>%
    tidyr::drop_na() %>%
    dplyr::count(.data$x, .data$group, .data$color, name = "y") %>%
    dplyr::group_by(.data$group) %>%
    dplyr::group_map(
      ~ create_plotly_barchart(
        plot_data = .,
        color_col = "color",
        text_col = "y",
        xlab = .y$group,
        showlegend = FALSE,
        palette = palette
      )
    ) %>%
    plotly::subplot(nrows = 1, shareX = TRUE, shareY = TRUE) %>%
    plotly::layout(yaxis = list(title = "Count"))
}

#' Create Plotly Barchart
#'
#' @param plot_data A dataframe
#' @param x_col A string
#' @param y_col A string
#' @param color_col A string or NA
#' @param key_col A string or NA
#' @param text_col A string or NA
#' @param xlab A string
#' @param ylab A string
#' @param title A string
#' @param showlegend True or False
#' @param palette A name of a viridis() palette
#'
#' @importFrom magrittr %>%
create_plotly_barchart <- function(
    plot_data,
    x_col = "x",
    y_col = "y",
    color_col = x_col,
    key_col = x_col,
    text_col = x_col,
    xlab = "",
    ylab = "",
    title = "",
    showlegend = TRUE,
    palette = "viridis"
) {

  select_cols <- c(
    "x"     = x_col,
    "y"     = y_col,
    "color" = color_col,
    "key"   = key_col,
    "text"  = text_col
  )

  plot_data <- dplyr::select(plot_data, dplyr::all_of(select_cols))
  bar_colors <- get_viridis_colors_from_tbl(plot_data, palette)

  p <- plotly::plot_ly(
    plot_data,
    x = ~x,
    y = ~y,
    color = ~color,
    text = ~text,
    textposition = "none",
    key = ~key,
    type = "bar",
    colors = bar_colors,
    hoverinfo = "text"
  ) %>%
    plotly::layout(
      title = title,
      xaxis = list(title = xlab),
      yaxis = list(title = ylab),
      barmode = "stack",
      showlegend = showlegend
    )
  return(p)
}
