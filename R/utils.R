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
get_viridis_colors_from_tbl <- function(table, pallete = "viridis") {
  table %>%
    dplyr::select("color") %>%
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
