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

#' Barchart Module Server
#'
#' @param id A shiny id
#' @param data A data frame
#' @param config A named list. All values must be columns in data:
#'  - "x_attribute" (required)
#'  - "color_attribute"
#'  - "group_attribute"
#'
#' @export
barchart_module_server <- function(id, data, config){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$plot <- plotly::renderPlotly({
        shiny::req(data(), config())

        plot_is_grouped <- !is.null(config()$group_attribute)
        plot_is_stacked <- !is.null(config()$color_attribute)

        if(plot_is_grouped && plot_is_stacked){
          return(create_stacked_grouped_barchart(data(), config()))
        } else if(plot_is_grouped){
          return(create_grouped_barchart(data(), config()))
        } else if (plot_is_stacked) {
          return(create_stacked_barchart(data(), config()))
        } else{
          return(create_standard_barchart(data(), config()))
        }
      })
    }
  )
}
