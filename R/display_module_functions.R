#' Validate Plot Config
#'
#' @param config The config being tested, a named list
#' @param error_prefix The prefix for the error message
validate_plot_config <- function(
    config,
    error_prefix = "Config malformed: "
) {
  if (is.null(config)) stop(error_prefix, "is NULL")
  if (length(config) == 0) stop(error_prefix, "is empty")
  if (length(config) == 1) {
    if (is.na(config)) stop(error_prefix, "is NA")
  }
  if (is.null(names(config))) stop(error_prefix, "is not named")
  if (is.null(config[["name"]])) stop(error_prefix, "has no name")
  if (is.null(config[["entity"]])) stop(error_prefix, "has no entity")
  missing_plot_type <- !any(
    get_plot_table()$plot_type %in% names(config)
  )
  if (missing_plot_type) stop(error_prefix, "missing an allowed plot type")
}
