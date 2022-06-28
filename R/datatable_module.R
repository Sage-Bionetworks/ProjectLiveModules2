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
#' @param config A shiny::reactive that returns a list
#' @param data A shiny::reactive that returns A data frame.
#' @param do_plot A shiny::reactive that returns a logical.
#'
#' @export
datatable_module_server <- function(
    id, config, data, do_plot = shiny::reactive(TRUE)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      output$datatable <- DT::renderDataTable({
        shiny::req(data(), do_plot())
        data()
      })

      return(TRUE)
    }
  )
}
