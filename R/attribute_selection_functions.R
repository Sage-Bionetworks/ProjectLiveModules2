#' Validate Attribute Config
#'
#' Checks that the config has names and has at least one value.
#'
#' @param config A named list
validate_attribute_config <- function(config) {
  malformed_config <- any(
    is.null(names(config)),
    length(config) == 0
  )
  if (malformed_config) stop("malformed config")
}

#' Validate Attribute Name
#'
#' Checks that the name is a length one character vector
#'
#' @param name A string
validate_attribute_name <- function(name) {
  malformed_name <- any(
    length(name) != 1,
    !is.character(name)
  )
  if (malformed_name) stop("malformed attribute name: ", name)
}

#' Validate Attribute Choices
#'
#' Checks that the choices have names and are at least length 1
#'
#' @param choices A
validate_attribute_choices <- function(choices) {
  malformed_choices <- any(
    is.null(names(choices)),
    length(choices) == 0,
    !is.character(choices)
  )
  if (malformed_choices) stop("malformed attribute choices")
}
