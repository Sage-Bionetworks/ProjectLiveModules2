server <- function(input, output) {

  library(magrittr)

  data <- shiny::reactive(readRDS("../../../tests/testthat/RDS/data.rds"))
  synapse_token <- yaml::read_yaml("../../../local_config.yml")$synapse_token
  config_list <- shiny::reactive(
    jsonlite::read_json("../../../tests/testthat/JSON/test.json")$plots
  )

  admin_module_server(
    "admin",
    config_list,
    data,
    synapse_token
  )

}
