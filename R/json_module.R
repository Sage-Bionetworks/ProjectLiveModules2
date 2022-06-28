#' Json Module UI
#'
#' @param id A shiny id
#'
#' @export
json_module_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectInput(
      ns("config_method_choice"),
      label = "Select Config Method.",
      choices = c(
        "Start From Scratch" = "none",
        # TODO: implement synapse API
        # "Download JSON from synapse" = "synapse",
        "Upload JSON from workstation" = "upload"
      )
    ),
    shiny::conditionalPanel(
      condition = "input.config_method_choice == 'synapse'",
      shiny::textInput(
        ns("file"),
        label = "Enter synapse id for JSON file",
        placeholder = "syn"
      ),
      ns = ns
    ),
    shiny::conditionalPanel(
      condition = "input.config_method_choice == 'upload'",
      shiny::fileInput(
        ns("json_upload"),
        "Choose CSV File",
        multiple = FALSE,
        accept = c(".json")
      ),
      ns = ns
    ),
    shiny::uiOutput(ns("config_selection_ui")),
  )
}


#' Json Module Server
#'
#' @param id A shiny id
#'
#' @export
json_module_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      config_list <- shiny::reactive({
        shiny::req(
          input$config_method_choice == "upload",
          input$json_upload$datapath
        )
        lst <-
          input$json_upload$datapath %>%
          jsonlite::read_json() %>%
          purrr::pluck("plots")
      })

      config_names <- shiny::reactive({
        shiny::req(config_list())
        purrr::map_chr(config_list(), "name")
      })

      output$config_selection_ui <- shiny::renderUI({
        shiny::req(config_names())

        shiny::selectInput(
          ns("config_choice"),
          label = "Select Config",
          choices = config_names()
        )
      })

      selected_config <- shiny::reactive({
        if (input$config_method_choice == "none") return(NULL)
        shiny::req(config_list(), input$config_choice)
        lst <- purrr::keep(
          config_list(),
          function(x) x$name == input$config_choice
        )
        if (length(lst) == 0L) stop("No matching configs")
        if (length(lst) > 1L) stop("Too many matching configs")
        config <- lst[[1]]
        malformed_config <- any(
          is.null(names(config)),
          length(config) == 0
        )
        if (malformed_config) stop("Selected config is malformed.")
        return(config)
      })

      return(selected_config)
    }
  )
}
