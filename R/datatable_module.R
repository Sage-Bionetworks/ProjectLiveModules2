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
    title = shiny::textOutput(ns("box_title")),
    DT::dataTableOutput(ns("datatable"))
  )

}

#' Data Table Module UI
#'
#' @param id A shiny id
#' @param config A shiny::reactive that returns a named list:
#'  - "name": a title for the shinydashboard::box
#' @param data A shiny::reactive that returns A data frame.
#'
#' @export
datatable_module_server <- function(id, config, data){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      box_title <- shiny::reactive({
        if(is.null(config()$name)) title <- "Datatable"
        else title <- config()$name
        return(title)
      })

      output$box_title <- shiny::renderText(box_title())

      output$datatable <- DT::renderDataTable({
        shiny::req(data())
        data()
      })

    }
  )
}
