#' Admin Piechart Module UI
#'
#' @param id A shiny id
admin_piechart_module_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shinydashboard::box(
      width = 12,
      solidHeader = TRUE,
      status = "warning",
      title = "Piechart Controls",
      attribute_sel_module_ui(ns("label_attribute"))
    )
  )

}


#' Admin Piechart Module Server
#'
#' @param id A shiny id
#' @param data A shiny::reactive that returns a data frame
#' @param input_config A shiny::reactive that returns a named list or Null.
admin_piechart_module_server <- function(id, data, input_config) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      #TODO: replace this with schematic API call
      column_choices <- shiny::reactive({
        shiny::req(data())
        choices <- colnames(data())
        names <- stringr::str_to_title(colnames(data()))
        purrr::set_names(choices, names)
      })

      label_attribute <- attribute_sel_module_server(
        "label_attribute",
        config            = input_config,
        attribute_name    = shiny::reactive("label_attribute"),
        attribute_choices = column_choices,
        ui_label          = shiny::reactive("Select label attribute")
      )

      output_config <- shiny::reactive({
        shiny::req(label_attribute())
        config <-
          list("label_attribute" = label_attribute()) %>%
          purrr::discard(., . == "none")
        return(config)
      })

      return(output_config)
    }
  )
}
