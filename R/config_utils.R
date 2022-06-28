#' Get Plot Type
#'
#' Determines the plot type from the config
#'
#' @param config A named list with attribute name of "plot_type"
get_plot_type <- function(config) {
  if ("barchart" %in% names(config)) plot_type <- "barchart"
  else if ("datatable" %in% names(config)) plot_type <- "datatable"
  else stop("Could not determine plot type.")
  return(plot_type)
}
