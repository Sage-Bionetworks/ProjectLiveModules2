#' Create Plot Config
#'
#' @param attributes A named list
#' @param plot_type A string
create_plot_config <- function(attributes, plot_type) {
  plot_table <- get_plot_table(plot_type)
  required   <- unlist(plot_table$required_attributes)
  optional   <- unlist(plot_table$optional_attributes)
  attributes <- attributes[c(required, optional)] %>%
    purrr::discard(., . == "none")
}


#' Get Plot Table
#'
#' This table has various data used specifically for each plot type
#'
#' @param type A string
get_plot_table <- function(type = NULL) {
  table <- dplyr::tibble(
    "plot_type" = c("barchart", "piechart", "datatable"),
    "display_name" = c("Barchart", "Piechart", "Data Table"),
    "required_attributes" = list(
      list("x_attribute"),
      list("label_attribute"),
      list()
    ),
    "optional_attributes" = list(
      list("color_attribute", "group_attribute"),
      list(),
      list()
    ),
    "plot_function" = list(create_barchart, create_piechart, NULL),
    "display_ui_module" = list(
      plotly_module_ui, plotly_module_ui, datatable_module_ui
    ),
    "display_server_module" = list(
      plotly_module_server, plotly_module_server, datatable_module_server
    ),
    "admin_ui_module" = list(
      admin_barchart_module_ui,
      admin_piechart_module_ui,
      admin_datatable_module_ui
    ),
    "admin_server_module" = list(
      admin_barchart_module_server,
      admin_piechart_module_server,
      admin_datatable_module_server
    )
  )
  if (!is.null(type)) {
    table <- dplyr::filter(table, .data$plot_type == type)
  }
  return(table)
}


#' Get Plot Type
#'
#' Determines the plot type from the config
#'
#' @param config A named list with attribute name of "plot_type"
get_plot_type <- function(config) {
  if ("barchart" %in% names(config)) plot_type <- "barchart"
  else if ("piechart" %in% names(config)) plot_type <- "piechart"
  else if ("datatable" %in% names(config)) plot_type <- "datatable"
  else stop("Could not determine plot type.")
  return(plot_type)
}

#' Get Value From List
#'
#' Gets the value from the list, or returns the default value if unable
#'
#' @param lst A named list
#' @param value The name of the value
#' @param default The default return value
get_value_from_list <- function(lst, value, default = NA) {
  return_default <- any(
    is.na(lst),
    is.na(value),
    is.na(lst[[value]]),
    is.null(lst),
    is.null(value),
    is.null(lst[[value]])
  )
  if (return_default) return(default)
  else return(lst[[value]])
}


# ploting utils ----

#' Get Viridis Colors From Tbl
#'
#' @param table A dataframe with column "color"
#' @param pallete A name of a viridis() pallete
#' @param column A name fo a column
get_viridis_colors_from_tbl <- function(
    table,
    pallete = "viridis",
    column = "color"
) {
  table %>%
    dplyr::select(dplyr::all_of(column)) %>%
    dplyr::n_distinct() %>%
    viridis::viridis_pal(option = pallete)()
}


#' Get Viridis Palette Options
#'
#' Returns the list of viridis palletes
get_viridis_palette_options <- function() {
  c(
    "magma",
    "inferno",
    "plasma",
    "viridis",
    "cividis",
    "rocket",
    "mako",
    "turbo"
  ) %>%
    purrr::set_names(stringr::str_to_title(.))
}
