#' Display Module UI
#'
#' @param id A shiny id
#'
#' @export
display_module_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::conditionalPanel(
      condition = "output.plot_type == 'datatable'",
      datatable_module_ui(ns("datatable")),
      ns = ns
    ),
    shiny::conditionalPanel(
      condition = "output.plot_type == 'barchart'",
      barchart_module_ui(ns("barchart")),
      ns = ns
    )
  )

}

#' Display Module Server
#'
#' @param id A shiny id
#' @param data A shiny::reactive that returns a named list of data frames.
#' @param config A shiny::reactive that returns a named list.
#'
#' @export
display_module_server <- function(id, config, data){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      selected_data <- shiny::reactive({
        shiny::req(data(), config())
        purrr::pluck(data(), config()$entity)
      })

      output$plot_type <- shiny::reactive(config()$type)

      shiny::outputOptions(
        output,
        "plot_type",
        suspendWhenHidden = FALSE
      )

      barchart_module_server("barchart", selected_data, config)
      datatable_module_server("datatable", selected_data, config)
    }
  )
}
