validate_attribute_config <- function(config){
  malformed_config <- any(
    is.null(names(config)),
    length(config) == 0
  )
  if(malformed_config) stop("malformed config")
}

validate_attribute_name <- function(name, config){
  malformed_name <- any(
    length(name) != 1,
    !is.character(name)
  )
  if(malformed_name) stop("malformed attribute name: ", name)
  if(!is.null(config)){
    if(!name %in% names(config)){
      stop("attribute name: ", name, " not in config names")
    }
  }
}

validate_attribute_choices <- function(choices){
  malformed_choices <- any(
    is.null(names(choices)),
    length(choices) == 0
  )
  if(malformed_choices) stop("malformed attribute choices")
}
