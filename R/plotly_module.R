#' Plotly Module UI
#'
#' @param id A shiny id
#'
#' @export
plotly_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("palette_ui")),
    plotly::plotlyOutput(ns("plot"))
  )
}

#' Plotly Module Server
#'
#' @param id A shiny id
#' @param data A shiny::reactive that returns A data frame.
#' @param config A shiny::reactive that returns a named list
#' @param ... unused arguments
#' @param plot_type The type of plot, see get_plot_table()$plot_type
#'
#' @export
plotly_module_server <- function(
    id,
    data,
    config,
    plot_type,
    ...
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      plot_table <- shiny::reactive({
        shiny::req(plot_type())
        dplyr::filter(get_plot_table(), .data$plot_type == plot_type())
      })

      validated_config <- shiny::reactive({
        shiny::req(config())
        if (!shiny::is.reactive(config)) stop("config is not reactive")
        validate_plotly_config(
          config(),
          unlist(plot_table()$required_attributes),
          unlist(plot_table()$optional_attributes)
        )
        return(config())
      })

      validated_data <- shiny::reactive({
        shiny::req(data())
        if (!shiny::is.reactive(data)) stop("data is not reactive")
        validate_plotly_data(data())
        return(data())
      })

      output$palette_ui <- shiny::renderUI({
        shiny::selectInput(
          inputId  = ns("palette_choice"),
          label    = "Select palette for plot",
          choices  = get_viridis_palette_options()
        )
      })

      plot_function <- shiny::reactive({
        shiny::req(plot_table())
        plot_table()$plot_function[[1]]
      })

      plot <- shiny::reactive({
        shiny::req(
          plot_function(),
          validated_config(),
          validated_data(),
          input$palette_choice
        )
        plot_function()(
          validated_data(),
          validated_config(),
          input$palette_choice
        )
      })

      output$plot <- plotly::renderPlotly(plot())

      return(TRUE)
    }
  )
}
