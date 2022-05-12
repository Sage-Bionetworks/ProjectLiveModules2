synapse_get <- function(
    url = "https://repo-prod.prod.sagebase.org/repo/v1/entity/",
    id,
    auth) {
  if (is.null(id)) stop("id cannot be NULL")
  req_url <- file.path(url, id)
  req <- httr::GET(
    req_url,
    httr::add_headers(Authorization = paste0("Bearer ", auth))
  )

  # Send error if unsuccessful query
  status <- httr::http_status(req)
  if (status$category != "Success") stop(status$message)

  cont <- httr::content(req)
  dplyr::bind_rows(cont)
}
