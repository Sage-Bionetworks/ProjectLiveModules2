#' Admin Module UI
#'
#' @param id A shiny id
#'
#' @export
admin_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        width = 12,
        solidHeader = TRUE,
        status = "warning",
        title = "Controls",
        json_module_ui(ns("json")),
        shiny::uiOutput(ns("name_selection_ui")),
        attribute_sel_module_ui(ns("entity_choice")),
        attribute_sel_module_ui(ns("display_choice"))
      )
    ),
    shiny::conditionalPanel(
      condition = "output.display_choice == 'barchart'",
      admin_barchart_module_ui(ns("barchart")),
      ns = ns
    ),
    shiny::conditionalPanel(
      condition = "output.display_choice == 'piechart'",
      admin_barchart_module_ui(ns("piechart")),
      ns = ns
    ),
    shiny::conditionalPanel(
      condition = "output.display_choice == 'datatable'",
      admin_datatable_module_ui(ns("datatable")),
      ns = ns
    ),
    display_module_ui(ns("display")),
    shiny::downloadButton(ns("download_json"), "Download JSON"),
  )

}


#' Admin Module Server
#'
#' @param id A shiny id
#' @param data a shiny::reactive that returns a named list of data frames
#'
#' @export
admin_module_server <- function(id, data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # json upload ----
      selected_input_config <- json_module_server("json")

      # name selection ----
      name_selection_default <- shiny::reactive({
        if (is.null(selected_input_config())) {
          default <- ""
        } else {
          default <- selected_input_config()$name
        }
        return(default)
      })

      output$name_selection_ui <- shiny::renderUI({
        shiny::req(!is.null(name_selection_default()))
        shiny::textInput(
          ns("name_choice"),
          label = "Pick a unique name for your plot.",
          value = name_selection_default()
        )
      })

      # entity (component) selection ----
      entity_choice <- attribute_sel_module_server(
        "entity_choice",
        config            = selected_input_config,
        ui_label          = shiny::reactive("Select Items to plot."),
        attribute_name    = shiny::reactive("entity"),
        attribute_choices = shiny::reactive(
          purrr::set_names(names(data()), names(data()))
        )
      )

      # display (plot type) selection ----
      display_choice <- attribute_sel_module_server(
        "display_choice",
        config            = selected_input_config,
        ui_label          = shiny::reactive("Select How to display items."),
        attribute_name    = shiny::reactive("type"),
        attribute_choices = shiny::reactive(
          c("Barchart" = "barchart", "Data Table" = "datatable")
        )
      )

      output$display_choice <- shiny::reactive(display_choice())

      shiny::outputOptions(
        output,
        "display_choice",
        suspendWhenHidden = FALSE
      )

      # rest ----

      selected_data <- shiny::reactive({
        shiny::req(entity_choice())
        purrr::pluck(data(), entity_choice())
      })

      barchart_config <- admin_barchart_module_server(
        "barchart",
        selected_data,
        selected_input_config
      )

      piechart_config <- admin_piechart_module_server(
        "piechart",
        selected_data,
        selected_input_config
      )

      datatable_config <- admin_datatable_module_server(
        "datatable",
        selected_input_config
      )

      plot_config <- shiny::reactive({
        shiny::req(display_choice())
        if (display_choice() == "barchart") return(barchart_config())
        else if (display_choice() == "piechart") return(piechart_config())
        else if (display_choice() == "datatable") return(datatable_config())
      })

      output_config <- shiny::reactive({
        shiny::req(
          !is.null(entity_choice()),
          !is.null(input$name_choice),
          !is.null(plot_config())
        )

        config <- list(
          "entity" = entity_choice(),
          "name" = input$name_choice
        )
        if (display_choice() == "barchart") {
          config <- c(
            config,
            list("barchart" = plot_config())
          )
        } else if (display_choice() == "piechart") {
          config <- c(
            config,
            list("piechart" = plot_config())
          )
        } else if (display_choice() == "datatable") {
          config <- c(
            config,
            list("datatable" = plot_config())
          )
        }
        return(config)
      })

      display_module_server(
        id = "display",
        config = output_config,
        data = data
      )

      output$download_json <- shiny::downloadHandler(
        filename = function() "test.json",
        content = function(con) {
          writeLines(
            jsonlite::toJSON(output_config()),
            con
          )
        }
      )


    }
  )
}
