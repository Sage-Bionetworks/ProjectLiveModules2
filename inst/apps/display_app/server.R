server <- function(input, output) {

  library(magrittr)

  config <- jsonlite::read_json("../../../tests/testthat/JSON/test.json")$plots
  data <- readRDS("../../../tests/testthat/RDS/data.rds")
  module_ids <- config %>%
    length() %>%
    seq(1, .) %>%
    stringr::str_c("plot", .)

  output$output_module_ui <- shiny::renderUI({
    purrr::map(module_ids, display_module_ui)
  })

  purrr::walk2(
    module_ids,
    config,
    ~display_module_server(.x, shiny::reactive(.y), shiny::reactive(data))
  )

}
