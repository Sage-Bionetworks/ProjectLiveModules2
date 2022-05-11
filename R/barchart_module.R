#' Barchart Module UI
#'
#' @param id A shiny id
#'
#' @export
barchart_module_ui <- function(id){
  ns <- shiny::NS(id)

  shinydashboard::box(
    width = 12,
    solidHeader = TRUE,
    status = "primary",
    title = "Barchart",
    plotly::plotlyOutput(ns("plot"))
  )

}

#' Barchart Module UI
#'
#' @param id A shiny id
#' @param data A data frame
#' @param config A named list
#'
#' @export
barchart_module_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$plot <- plotly::renderPlotly({
        shiny::req(data(), config())
        if(config()$group_attribute != "none" && config()$color_attribute != "none"){
          return(create_stacked_grouped_barchart(data(), config()))
        } else if(config()$group_attribute != "none"){
          return(create_grouped_barchart(data(), config()))
        } else if (config()$color_attribute != "none") {
          return(create_stacked_barchart(data(), config()))
        } else{
          return(create_standard_barchart(data(), config()))
        }
      })
    }
  )
}
