#' Data Table Module UI
#'
#' @param id A shiny id
#'
#' @export
datatable_module_ui <- function(id) {
  ns <- shiny::NS(id)
  DT::dataTableOutput(ns("datatable"))
}

#' Data Table Module UI
#'
#' @param id A shiny id
#' @param data A shiny::reactive that returns A data frame.
#' @param ... unused arguments
#'
#' @export
datatable_module_server <- function(
    id,
    data,
    ...
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      output$datatable <- DT::renderDataTable({
        shiny::req(data())
        data()
      })

      return(TRUE)
    }
  )
}
