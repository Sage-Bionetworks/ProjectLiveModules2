#' Admin Data Table Module UI
#'
#' @param id A shiny id
#'
#' @export
admin_datatable_module_ui <- function(id) {

  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      solidHeader = TRUE,
      status = "warning",
      title = "Data Table Controls"
    )
  )
}


#' Admin Data Table Module Server
#'
#' @param id A shiny id
#' @param data A shiny::reactive that returns a data frame
#' @param input_config A shiny::reactive that returns a named list or Null.
#'
#' @export
admin_datatable_module_server <- function(id, data, input_config) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      output_config <- shiny::reactive(list())

      return(output_config)

    }
  )
}
