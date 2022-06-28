server <- function(input, output) {

  library(magrittr)
  verbose <- FALSE
  config <- jsonlite::read_json("../../../tests/testthat/JSON/test.json")$plots
  data_object <- readRDS("../../../tests/testthat/RDS/data.rds")
  if (verbose) {
    print(config)
    print(data_object)
  }
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
    ~display_module_server(
      .x,
      shiny::reactive(.y),
      shiny::reactive(data_object)
    )
  )

}
