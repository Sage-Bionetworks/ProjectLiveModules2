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
#' @param data A data frame
#' @param config A named list. All values must be columns in data:
#'  - "name": shinydashboard::box box title
#'
#' @export
datatable_module_server <- function(id, config, data){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      box_title <- shiny::reactive({
        if(is.null(config()$name)) title <- "Barchart"
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
