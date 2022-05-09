create_standard_barchart <- function(data, config){
  data %>%
    dplyr::select("x" = config$x_attribute) %>%
    tidyr::drop_na() %>%
    dplyr::count(.data$x, name = "y") %>%
    plotly_bar(text_col = "y")
}

create_stacked_barchart <- function(data, config){
  data %>%
    dplyr::select(
      "x" = config$x_attribute,
      "color" = config$color_attribute
    ) %>%
    tidyr::drop_na() %>%
    dplyr::count(.data$x, .data$color, name = "y") %>%
    plotly_bar(color_col = "color", text_col = "y")
}

create_grouped_barchart <- function(data, config){
  data %>%
    dplyr::select(
      "x" = config$x_attribute,
      "group" = config$group_attribute
    ) %>%
    tidyr::drop_na() %>%
    dplyr::count(.data$x, .data$group, name = "y") %>%
    dplyr::group_by(.data$group) %>%
    dplyr::group_map(
      ~ plotly_bar(
        plot_data = .,
        text_col = "y",
        xlab = .y$group,
        showlegend = FALSE
      )
    ) %>%
    plotly::subplot(nrows = 1, shareX = TRUE, shareY = TRUE) %>%
    plotly::layout(yaxis = list(title = "Count"))
}

create_stacked_grouped_barchart <- function(data, config){
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
      ~ plotly_bar(
        plot_data = .,
        color_col = "color",
        text_col = "y",
        xlab = .y$group,
        showlegend = FALSE
      )
    ) %>%
    plotly::subplot(nrows = 1, shareX = TRUE, shareY = TRUE) %>%
    plotly::layout(yaxis = list(title = "Count"))
}



#' Plotly Bar
#'
#' @param plot_data A dataframe
#' @param x_col A string
#' @param y_col A string
#' @param error_col A string or NA
#' @param color_col A string or NA
#' @param key_col A string or NA
#' @param text_col A string or NA
#' @param xlab A string
#' @param ylab A string
#' @param title A string
#' @param source_name A string or NULL
#' @param bar_colors A string or NULL
#'
#' @importFrom magrittr %>%
#'
#' @export
plotly_bar <- function(
    plot_data,
    x_col = "x",
    y_col = "y",
    color_col = x_col,
    key_col = x_col,
    text_col = x_col,
    xlab = "",
    ylab = "",
    title = "",
    source_name = NULL,
    bar_colors = NULL,
    format_func = NULL,
    showlegend = TRUE
) {

  select_cols <- c(
    "x"     = x_col,
    "y"     = y_col,
    "color" = color_col,
    "key"   = key_col,
    "text"  = text_col
  )

  plot_data <- dplyr::select(plot_data, dplyr::all_of(select_cols))

  if (is.null(bar_colors)) {
    bar_colors <- plot_data %>%
      dplyr::select("color") %>%
      dplyr::n_distinct() %>%
      viridis::viridis_pal(option = "D")()
  }

  p <- plotly::plot_ly(
    plot_data,
    x = ~x,
    y = ~y,
    color = ~color,
    text = ~text,
    textposition = 'none',
    key = ~key,
    type = "bar",
    source = source_name,
    colors = bar_colors,
    hoverinfo = "text"
  ) %>%
    plotly::layout(
      title = title,
      xaxis = list(title = xlab),
      yaxis = list(title = ylab),
      barmode = 'stack',
      showlegend = showlegend
    )

  return(p)
}
