#' Admin Module UI
#'
#' @param id A shiny id
#'
#' @export
admin_module_ui <- function(id){
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
        shiny::uiOutput(ns("entity_selection_ui")),
        shiny::uiOutput(ns("display_selection_ui")),
      )
    ),
    shiny::conditionalPanel(
      condition = "input.display_choice == 'datatable'",
      admin_datatable_module_ui(ns("datatable")),
      ns = ns
    ),
    shiny::conditionalPanel(
      condition = "input.display_choice == 'barchart'",
      admin_barchart_module_ui(ns("barchart")),
      ns = ns
    ),
    shiny::downloadButton(ns("download_json"), "Download JSON"),
  )

}


#' Admin Module Server
#'
#' @param id A shiny id
#' @param data a shiny::reactive that returns a named list of data frames
#'
#' @export
admin_module_server <- function(id, data){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns


      selected_input_config <- json_module_server("json")

      # other input ----

      name_selection_default <- shiny::reactive({
        if(is.null(selected_input_config())){
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

      display_selection_default <- shiny::reactive({
        if(is.null(selected_input_config())){
          default <- NA
        } else {
          default <- selected_input_config()$type
        }
        return(default)
      })

      output$display_selection_ui <- shiny::renderUI({
        shiny::req(!is.null(display_selection_default()))

        shiny::selectInput(
          ns("display_choice"),
          label = "Select How to display items.",
          choices = list(
            "Barchart" = "barchart",
            "Data Table" = "datatable"
          ),
          selected = display_selection_default()
        )
      })

      entity_selection_default <- shiny::reactive({
        if(is.null(selected_input_config())){
          default <- NA
        }
        else {
          default <- selected_input_config()$entity
        }
        return(default)
      })

      output$entity_selection_ui <- shiny::renderUI({
        shiny::req(!is.null(entity_selection_default()))

        shiny::selectInput(
          ns("entity_choice"),
          label = "Select Items to plot.",
          choices = names(data()),
          selected = entity_selection_default()
        )
      })

      # rest ----

      selected_data <- shiny::reactive({
        shiny::req(input$entity_choice)
        purrr::pluck(data(), input$entity_choice)
      })

      barchart_config <- admin_barchart_module_server(
        "barchart",
        selected_data,
        selected_input_config,
        shiny::reactive(input$entity_choice),
        shiny::reactive(input$name_choice)
      )

      datatable_config <- admin_datatable_module_server(
        "datatable",
        selected_data,
        selected_input_config,
        shiny::reactive(input$entity_choice),
        shiny::reactive(input$name_choice)
      )

      config <- shiny::reactive({
        shiny::req(input$display_choice)
        if(input$display_choice == "barchart") return(barchart_config())
        else if(input$display_choice == "datatable") return(datatable_config())
      })

      json_config <- shiny::reactive({
        jsonlite::toJSON(config())
      })

      output$download_json <- shiny::downloadHandler(
        filename = function() "test.json",
        content = function(con) writeLines(
          jsonlite::toJSON(config()),
          con
        )
      )

      return(config)

    }
  )
}
