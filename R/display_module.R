#' Display Module UI
#'
#' @param id A shiny id
#'
#' @export
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
#' @param plot_config A shiny::reactive that returns a named list:
#'  - "type": one of c("datatable", "barchart")
#'  - "entity": a name in data
#'  -  One of ("barchart", "datatable"). See barchart_module_server() and
#'  datatable_module_server()
#' @param data A shiny::reactive that returns a named list of data frames.

#'
#' @export
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

      output$plot_module_ui <- shiny::renderUI({
        if (plot_type() == "barchart") {
          barchart_module_ui(ns("barchart"))
        } else if (plot_type() == "piechart") {
          plotly_module_ui(ns("piechart"))
        } else if (plot_type() == "datatable") {
          datatable_module_ui(ns("datatable"))
        } else {
          stop("Unrecognized plot type: ", plot_type())
        }
      })

      barchart_module_server(
        "barchart",
        plot_config,
        selected_data,
        shiny::reactive(plot_type() == "barchart")
      )
      plotly_module_server(
        "piechart",
        config = plot_config,
        data = selected_data,
        plot_function = shiny::reactive(create_piechart),
        required_config_attrbutes = shiny::reactive("label_attribute"),
        do_plot = shiny::reactive(plot_type() == "piechart")
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
