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
        shiny::uiOutput(ns("entity_selection_ui")),
        shiny::selectInput(
          ns("display_choice"),
          label = "Select How to display items.",
          choices = list("Barchart", "Data Table")
        )
      )
    ),
    shiny::conditionalPanel(
      condition = "input.display_choice == 'Data Table'",
      admin_datatable_module_ui(ns("datatable")),
      ns = ns
    ),
    shiny::conditionalPanel(
      condition = "input.display_choice == 'Barchart'",
      admin_barchart_module_ui(ns("barchart")),
      ns = ns
    ),
    shiny::downloadButton(ns("download_json"), "Download JSON"),
  )

}


#' Admin Module Server
#'
#' @param id A shiny id
#' @param data A named list of data frames
#'
#' @export
admin_module_server <- function(id, data){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$entity_selection_ui <- shiny::renderUI({
        shiny::selectInput(
          ns("entity_choice"),
          label = "Select Items to plot.",
          choices = names(data)
        )
      })

      selected_data <- shiny::reactive({
        shiny::req(input$entity_choice)
        purrr::pluck(data, input$entity_choice)
      })

      barchart_config <- admin_barchart_module_server(
        "barchart",
        selected_data,
        shiny::reactive(input$entity_choice)
      )

      datatable_config <- admin_datatable_module_server(
        "datatable",
        selected_data,
        shiny::reactive(input$entity_choice)
      )

      config <- shiny::reactive({
        shiny::req(input$display_choice)
        if(input$display_choice == "Barchart") return(barchart_config())
        else if(input$display_choice == "Data Table") return(datatable_config())
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
