#' Display Module UI
#'
#' @param id A shiny id
display_module_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      title = shiny::textOutput(ns("box_title")),
      shiny::uiOutput(ns("plot_module_ui"))
    )
  )
}

#' Display Module Server
#'
#' @param id A shiny id
#' @param config A shiny::reactive that returns a named list:
#'  - "type": one of c("datatable", "barchart")
#'  - "entity": a name in data
#'  -  One of ("barchart", "datatable"). See barchart_module_server() and
#'  datatable_module_server()
#' @param data A shiny::reactive that returns a named list of data frames.
display_module_server <- function(id, config, data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      validated_config <- shiny::reactive({
        if (!shiny::is.reactive(config)) stop("config is not reactive")
        validate_plot_config(config())
        return(config())
      })

      validated_data <- shiny::reactive({
        if (!shiny::is.reactive(data)) stop("data is not reactive")
        data <- data()
        malformed_data <- any(
          length(data) == 0,
          is.null(names(data))
        )
        if (malformed_data) stop("data is malformed")
        return(data)
      })

      selected_data <- shiny::reactive({
        shiny::req(validated_data(), validated_config())
        purrr::pluck(validated_data(), validated_config()$entity)
      })

      box_title <- shiny::reactive({
        shiny::req(config())
        config()$name
      })

      output$box_title <- shiny::renderText(box_title())

      plot_type <- shiny::reactive({
        shiny::req(validated_config())
        get_plot_type(validated_config())
      })

      plot_config <- shiny::reactive({
        shiny::req(validated_config(), plot_type())
        config <- validated_config()[[plot_type()]]
        return(config)
      })

      plot_function_row <- shiny::reactive({
        shiny::req(plot_type())
        dplyr::filter(get_plot_function_table(), .data$plot_type == plot_type())
      })

      ui_module <- shiny::reactive({
        shiny::req(plot_function_row())
        column <- dplyr::pull(plot_function_row(), "display_ui_module")
        return(column[[1]])
      })

      output$plot_module_ui <- shiny::renderUI({
        shiny::req(ui_module())
        ui_module()(id = ns(plot_type()))
      })

      plotly_module_server(
        "barchart",
        config = plot_config,
        data = selected_data,
        plot_function = shiny::reactive(create_barchart),
        required_config_attrbutes = shiny::reactive(
          get_plot_attribute_config()$barchart$required
        ),
        optional_config_attributes = shiny::reactive(
          get_plot_attribute_config()$barchart$optional
        )
      )
      plotly_module_server(
        "piechart",
        config = plot_config,
        data = selected_data,
        plot_function = shiny::reactive(create_piechart),
        required_config_attrbutes = shiny::reactive(
          get_plot_attribute_config()$piechart$required
        ),
        optional_config_attributes = shiny::reactive(
          get_plot_attribute_config()$piechart$optional
        )
      )
      datatable_module_server(
        "datatable",
        plot_config,
        selected_data,
        shiny::reactive(plot_type() == "datatable")
      )

    }
  )
}
