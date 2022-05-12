#' Admin Data Table Module UI
#'
#' @param id A shiny id
#'
#' @export
admin_datatable_module_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(datatable_module_ui(ns("datatable")))
  )

}


#' Admin Data Table Module Server
#'
#' @param id A shiny id
#' @param data A data frame
#' @param entity A string
#'
#' @export
admin_datatable_module_server <- function(id, data, entity){
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      config <- shiny::reactive({
        shiny::req(entity())
        list(
          "type" = "datatable",
          "entity" = entity()
        )
      })

      datatable_module_server(
        id = "datatable",
        config = config,
        data = data
      )

      return(config)

    }
  )
}
