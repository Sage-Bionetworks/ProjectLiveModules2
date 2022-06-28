library(magrittr)

dates <- seq(
  lubridate::ymd("2021-04-07"),
  lubridate::ymd("2022-03-22"),
  by = "1 month"
)

date_estimates <- seq(
  lubridate::ymd("2021-07-01"),
  lubridate::ymd("2022-05-01"),
  by = "5 month"
)

assays       <- c("Assay1", "Assay2", "Assay3", NA)
species      <- c("Species1", "Species2", NA)
initiatives  <- c("Initiative1", "Initiative2")
access_types <- c("Public", "Private", NA)
file_formats <- c("jpg", "csv", "tsv")
milestones   <- c(1L, 2L, 3L)

n_studies <- 10

studies <- dplyr::tibble(
  "study_name"  = stringr::str_c(
    "Study", stringr::str_to_upper(letters[1:n_studies])
  ),
  "study_id"    = stringr::str_c("S", as.character(1:n_studies)),
  "initiative"  = sample(initiatives, n_studies,  replace = TRUE),
)

study_ids <- c(studies$study_id, "SX", NA)


n_files      <- 1000

files <- dplyr::tibble(
  "file_name"   = stringr::str_c("File", as.character(1:n_files)),
  "file_id"     = stringr::str_c("F", as.character(1:n_files)),
  "study_id"    = sample(study_ids, n_files,  replace = TRUE),
  "species"     = sample(species, n_files,  replace = TRUE),
  "assay"       = sample(assays, n_files,  replace = TRUE),
  "access_type" = sample(access_types, n_files,  replace = TRUE),
  "date"        = sample(dates, n_files,  replace = TRUE),
  "file_format" = sample(file_formats, n_files,  replace = TRUE),
) %>%
  dplyr::mutate("year" = lubridate::year(.data$date)) %>%
  dplyr::mutate("year" = as.factor(.data$year)) %>%
  dplyr::left_join(
    dplyr::select(studies, "study_id", "initiative"),
    by = "study_id"
  )


n_pubs      <- 1000

publications <- dplyr::tibble(
  "publication_name" = stringr::str_c("File", as.character(1:n_pubs)),
  "publication_id"   = stringr::str_c("F", as.character(1:n_pubs)),
  "study_id"         = sample(study_ids, n_pubs,  replace = TRUE),
  "assay"            = sample(assays, n_pubs,  replace = TRUE),
  "date"             = sample(dates, n_pubs,  replace = TRUE)
) %>%
  dplyr::mutate("year" = lubridate::year(.data$date)) %>%
  dplyr::mutate("year" = as.factor(.data$year))


n_tools <- 20

tools <- dplyr::tibble(
  "study_id"  = sample(study_ids, n_tools,  replace = TRUE),
  "tool_name" = stringr::str_c("Tool", as.character(1:n_tools)),
  "tool_id"   = stringr::str_c("T", as.character(1:n_tools)),
)


saveRDS(
  list(
    "Studies" = studies,
    "Files" = files,
    "Publications" = publications,
    "Tools" = tools
  ),
  "tests/testthat/RDS/data.rds"
)
