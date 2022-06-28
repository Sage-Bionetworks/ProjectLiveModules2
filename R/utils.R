get_value_from_list <- function(lst, value, default = NA){
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
