server <- function(input, output) {

  library(magrittr)

  data <- shiny::reactive(readRDS("../../../tests/testthat/RDS/data.rds"))

  admin_module_server("admin", data)

}
