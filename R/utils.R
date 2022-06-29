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
