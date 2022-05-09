#' Data Table Module UI
#'
#' @param id A shiny id
#'
#' @export
datatable_module_ui <- function(id){
  ns <- shiny::NS(id)

  shinydashboard::box(
    width = 12,
    solidHeader = TRUE,
    status = "primary",
    title = "Data Table",
    DT::dataTableOutput(ns("datatable"))
  )

}

#' Data Table Module UI
#'
#' @param id A shiny id
#' @param data A data frame
#' @param config A named list
#'
#' @export
datatable_module_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$datatable <- DT::renderDataTable({
        shiny::req(data())
        data()
      })

    }
  )
}
