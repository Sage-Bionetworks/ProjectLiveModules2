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
        shiny::uiOutput(ns("config_selection_ui")),
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
#' @param data A named list of data frames
#'
#' @export
admin_module_server <- function(id, config_list, data, synapse_token){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # JSON input ----

      input_config_names <- shiny::reactive({
        shiny::req(config_list())
        purrr::map_chr(config_list(), "name")
      })

      output$config_selection_ui <- shiny::renderUI({
        shiny::req(input_config_names())

        shiny::selectInput(
          ns("config_choice"),
          label = "Select Config.",
          choices = input_config_names()
        )
      })

      selected_input_config <- shiny::reactive({
        shiny::req(config_list(), input$config_choice)
        lst <- purrr::keep(
          config_list(),
          function(x) x$name == input$config_choice
        )
        if(length(lst) == 0L) stop("No matching configs")
        if(length(lst) > 1L) stop("Too many matching configs")
        lst[[1]]

      })

      # other input ----

      #TODO: change
      display_selection_default <- shiny::reactive({
        if(TRUE) default <- selected_input_config()$type
        else default <- NA
      })

      output$display_selection_ui <- shiny::renderUI({
        shiny::req(display_selection_default())

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

      #TODO: change
      entity_selection_default <- shiny::reactive({
        if(TRUE) default <- selected_input_config()$entity
        else default <- NA
      })

      output$entity_selection_ui <- shiny::renderUI({
        shiny::req(entity_selection_default())

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
        shiny::reactive(input$entity_choice)
      )

      datatable_config <- admin_datatable_module_server(
        "datatable",
        selected_data,
        shiny::reactive(input$entity_choice)
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
