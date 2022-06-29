#' Validate Plotly Config
#'
#' @param config The config being tested, a named list
#' @param required_attrbutes The attributes the config must have
#' @param optional_attributes The attributes the config is allowed to have
#' @param error_prefix The prefix for the error message
validate_plotly_config <- function(
    config,
    required_attrbutes = c(),
    optional_attributes = c(),
    error_prefix = "Config malformed: "
) {
  if (is.null(config)) stop(error_prefix, "is NULL")
  if (length(config) == 0) stop(error_prefix, "is empty")
  if (length(config) == 1) {
    if (is.na(config)) stop(error_prefix, "is NA")
  }
  if (is.null(names(config))) stop(error_prefix, "is not named")
  validate_required_attributes(config, required_attrbutes, error_prefix)
  validate_allowed_attributes(
    config,
    required_attrbutes,
    optional_attributes,
    error_prefix
  )
}

#' Validate Required Attributes
#'
#' @param config The config being tested, a named list
#' @param required_attrbutes The attributes the config must have
#' @param error_prefix The prefix for the error message
validate_required_attributes <- function(
    config,
    required_attrbutes,
    error_prefix = "Config malformed: "
) {
  missing_attributes <- !all(required_attrbutes %in% names(config))
  if (missing_attributes) {
    stop(create_missing_attr_msg(
      names(config),
      required_attrbutes,
      error_prefix
    ))
  }
}

#' Create Missing Attribute Message
#'
#' @param config_names A list of names in the config
#' @param required_attrbutes The attributes the config must have
#' @param error_prefix The prefix for the error message
create_missing_attr_msg <- function(
    config_names,
    required_attrbutes,
    error_prefix = "Config malformed: "
) {
  stringr::str_c(
    error_prefix,
    "missing required attributes; ",
    "config has: ",
    stringr::str_c(config_names, collapse = ", "),
    " requires: ",
    stringr::str_c(required_attrbutes, collapse = ", ")
  )
}

#' Validate Allowed Attributes
#'
#' @param config The config being tested, a named list
#' @param required_attrbutes The attributes the config must have
#' @param optional_attributes The attributes the config is allowed to have
#' @param error_prefix The prefix for the error message
validate_allowed_attributes <- function(
    config,
    required_attrbutes,
    optional_attributes,
    error_prefix = "Config malformed: "
) {
  allowed_attributes <- c(required_attrbutes, optional_attributes)
  extra_attributes <- !all(names(config) %in% allowed_attributes)

  if (extra_attributes) {
    stop(create_extraneous_attr_msg(
      names(config),
      allowed_attributes,
      error_prefix
    ))
  }
}

#' Create Extraneous Attribute Message
#'
#' @param config_names A list of names from the config
#' @param allowed_attributes A list of attributes allowed for the config
#' @param error_prefix The prefix for the error message
create_extraneous_attr_msg <- function(
    config_names,
    allowed_attributes,
    error_prefix = "Config malformed: "
) {
  stringr::str_c(
    error_prefix,
    "extraneous attributes; ",
    "config has: ",
    stringr::str_c(config_names, collapse = ", "),
    "; allowed: ",
    stringr::str_c(allowed_attributes, collapse = ", ")
  )
}

#' Validate Plotly Data
#'
#' @param data A dataframe
#' @param error_prefix The prefix for the error message
validate_plotly_data <- function(
    data,
    error_prefix = "Data malformed: "
) {
  if (is.null(data)) stop(error_prefix, "is NULL")
  if (!"data.frame" %in% class(data)) stop(error_prefix, "is not a dataframe")
  if (ncol(data) == 0) stop(error_prefix, "has no columns")
  if (nrow(data) == 0) stop(error_prefix, "has no rows")
}
