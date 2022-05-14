#' Display Module UI
#'
#' @param id A shiny id
#'
#' @export
display_module_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      title = shiny::textOutput(ns("box_title")),
      shiny::conditionalPanel(
        condition = "output.plot_type == 'barchart'",
        barchart_module_ui(ns("barchart")),
        ns = ns
      ),
      shiny::conditionalPanel(
        condition = "output.plot_type == 'datatable'",
        datatable_module_ui(ns("datatable")),
        ns = ns
      )
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

#'
#' @export
display_module_server <- function(id, config, data){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      validated_config <- shiny::reactive({
        if(!shiny::is.reactive(config)) stop("config is not reactive")
        config <- config()

        malformed_config <- any(
          length(config) == 0,
          is.null(config[["name"]]),
          is.null(config[["entity"]]),
          is.null(config[["barchart"]]) && is.null(config[["datatable"]])
        )
        if(malformed_config) stop("config is malformed")
        return(config)
      })

      validated_data <- shiny::reactive({
        if(!shiny::is.reactive(data)) stop("data is not reactive")
        data <- data()
        malformed_data <- any(
          length(data) == 0,
          is.null(names(data))
        )
        if(malformed_data) stop("config is not malformed")
        return(data)
      })

      selected_data <- shiny::reactive({
        shiny::req(validated_data(),validated_config())
        purrr::pluck(validated_data(), validated_config()$entity)
      })

      box_title <- shiny::reactive({
        shiny::req(config())
        config()$name
      })

      output$box_title <- shiny::renderText(box_title())

      plot_type <- shiny::reactive({
        shiny::req(validated_config())
        config <- validated_config()
        if("barchart" %in% names(config)) plot_type <- "barchart"
        if("datatable" %in% names(config)) plot_type <- "datatable"
        return(plot_type)
      })

      output$plot_type <- shiny::reactive(plot_type())

      shiny::outputOptions(
        output,
        "plot_type",
        suspendWhenHidden = FALSE
      )

      plot_config <- shiny::reactive({
        shiny::req(validated_config(), plot_type())
        config <- validated_config()[[plot_type()]]
        return(config)
      })

      barchart_module_server(
        "barchart",
        plot_config,
        selected_data,
        shiny::reactive(plot_type() == "barchart")
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
