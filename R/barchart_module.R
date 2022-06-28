#' Barchart Module UI
#'
#' @param id A shiny id
#'
#' @export
barchart_module_ui <- function(id) {
  ns <- shiny::NS(id)
  plotly::plotlyOutput(ns("plot"))
}

#' Barchart Module Server
#'
#' @param id A shiny id
#' @param data A shiny::reactive that returns A data frame.
#' @param config A shiny::reactive that returns a named list:
#'  - "x_attribute": Column in data param (required)
#'  - "color_attribute": Column in data param
#'  - "group_attribute": Column in data param
#' @param do_plot A shiny::reactive that returns a logical.
#' @export
barchart_module_server <- function(
    id, config, data, do_plot = shiny::reactive(TRUE)
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      validated_config <- shiny::reactive({
        shiny::req(config(), do_plot())
        if (!shiny::is.reactive(config)) stop("config is not reactive")
        malformed_config <- any(
          length(config()) == 0,
          is.null(config()[["x_attribute"]])
        )
        if (malformed_config) stop("config is malformed")
        return(config())
      })

      validated_data <- shiny::reactive({
        shiny::req(data(), do_plot())
        if (!shiny::is.reactive(data)) stop("data is not reactive")
        malformed_data <- any(
          !tibble::is_tibble(data())
        )
        if (malformed_data) stop("config is malformed")
        return(data())
      })

      plot <- shiny::reactive({
        shiny::req(validated_config(), validated_data())
        create_barchart(validated_data(), validated_config())
      })

      output$plot <- plotly::renderPlotly(plot())

      return(TRUE)
    }
  )
}
