test_that("get_value_from_list", {
  lst <- list("name" = "value")

  expect_equal(
    get_value_from_list(lst, "name"),
    "value"
  )

  expect_true(is.na(
    get_value_from_list(NULL, "name")
  ))

  expect_equal(
    get_value_from_list(NULL, "name", "default"),
    "default"
  )
})


test_that("get_viridis_colors_from_tbl", {
  expect_equal(
    get_viridis_colors_from_tbl(dplyr::tibble("color" = c("a", "a", "b"))),
    c("#440154FF", "#FDE725FF")
  )
  expect_equal(
    get_viridis_colors_from_tbl(
      dplyr::tibble("color" = c("a", "a", "b")),
      "magma"
    ),
    c("#000004FF", "#FCFDBFFF")
  )
  expect_equal(
    get_viridis_colors_from_tbl(
      dplyr::tibble("color" = c("a", "a", "b")),
      "A"
    ),
    c("#000004FF", "#FCFDBFFF")
  )
})
