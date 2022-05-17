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
        validate_attribute_config(config())
        return(config())
      })

      validated_name <- shiny::reactive({
        if(!shiny::is.reactive(attribute_name)) {
          stop("attribute_name is not reactive")
        }
        shiny::req(attribute_name())
        validate_attribute_name(attribute_name(), validated_config())
        return(attribute_name())
      })

      validated_choices <- shiny::reactive({
        if(!shiny::is.reactive(attribute_choices)) {
          stop("attribute_choices is not reactive")
        }
        shiny::req(attribute_choices())
        choices <- attribute_choices()
        validate_attribute_choices(choices)
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
