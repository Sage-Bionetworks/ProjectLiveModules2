#' Attribute Selection Module UI
#'
#' @param id A shiny id
#'
#' @export
attribute_selection_module_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("attribute_ui"))
}


#' Attribute Selection Module Server
#'
#' @param id A shiny id
#' @param config A shiny::reactive that returns a named list or Null.
#' @param attribute_name A shiny::reactive that returns a string
#' @param attribute_choices A shiny::reactive that returns a named list
#' @param attribute_input_default A shiny::reactive that returns a string or NA
#' @param ui_label  A shiny::reactive that returns a string

#'
#' @export
attribute_selection_module_server <- function(
    id,
    config,
    attribute_name,
    attribute_choices,
    attribute_input_default = shiny::reactive(NA),
    ui_label = shiny::reactive("Select attribute")
  ){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # validate inputs ----
      validated_config <- shiny::reactive({
        if(!shiny::is.reactive(config)) stop("config is not reactive")
        if(is.null(config())) return(config())
        malformed_config <- any(
          !is.list(config()),
          is.null(names(config())),
          length(config()) == 0
        )
        if(malformed_config) stop("malformed config")
        config()
      })

      validated_name <- shiny::reactive({
        if(!shiny::is.reactive(attribute_name)) {
          stop("attribute_name is not reactive")
        }
        shiny::req(attribute_name())
        name <- attribute_name()
        malformed_name <- any(
          length(name) != 1,
          !is.character(name)
        )
        if(malformed_name) stop("malformed attribute name: ", name)
        if(!is.null(validated_config())){
          if(!name %in% names(validated_config())){
            stop("attribute name: ", name, " not in config names")
          }
        }
        return(name)
      })

      validated_choices <- shiny::reactive({
        if(!shiny::is.reactive(attribute_choices)) {
          stop("attribute_choices is not reactive")
        }
        shiny::req(attribute_choices())
        choices <- attribute_choices()
        malformed_choices <- any(
          is.null(names(choices)),
          length(choices) == 0
        )
        if(malformed_choices) stop("malformed attribute choices")
        return(choices)
      })

      # rest ----
      attribute_default <- shiny::reactive({
        get_value_from_list(
          validated_config(),
          validated_name(),
          attribute_input_default()
        )
      })

      output$attribute_ui <- shiny::renderUI({
        shiny::req(
          !is.null(attribute_default()),
          validated_choices()
        )
        shiny::selectInput(
          inputId  = ns("attribute_choice"),
          label    = ui_label(),
          choices  = validated_choices(),
          selected = attribute_default()
        )
      })

      attribute_choice <- shiny::reactive({
        input$attribute_choice
      })

      return(attribute_choice)
    }
  )
}
