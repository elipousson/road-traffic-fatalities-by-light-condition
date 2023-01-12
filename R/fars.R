# get data from https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars

year_range <- c(2011:2020)

# Download ZIP files with FARS data
purrr::walk(
  year_range,
  function(x) {
    crashapi::get_fars_zip(
      year = x, format = "csv",
      path = here::here("data"), read = FALSE
    )
    
    # Download unzip data files (each year in a different folder)
    unzip(here::here("data", glue::glue("FARS{x}NationalCSV.zip")),
          exdir = glue::glue("data/FARS{x}")
    )
  }
)


fars <- 
  purrr::map_dfr(
  year_range,
  function(x) {
    path <- paste0("data/FARS", x)

    file <-
      fs::dir_ls(
        path = paste0("data/FARS", x),
        regexp = "accident|ACCIDENT"
      )

    readr::read_csv(file) |>
      dplyr::select(
        dplyr::any_of(
          c("ST_CASE", "COUNTY", "STATE", "YEAR", "MONTH", "DAY", "HOUR", "MINUTE", "HARM_EV", "LGT_COND")
        )
      )
  }
)

purrr::walk(
  year_range,
  function(x) {
    # Download auxiliary ZIP files with FARS data
    crashapi::get_fars_zip(
      year = x, format = "csv",
      aux = TRUE,
      path = here::here("data"), read = FALSE
    )

    # Unzip the data
    unzip(here::here("data", glue::glue("FARS{x}NationalAuxiliaryCSV.zip")),
      exdir = here::here(glue::glue("data/FARS{x}_Aux"))
    )
  }
)

fars_aux <- purrr::map_dfr(
  year_range,
  function(x) {
    # Read and join the auxiliary crash and person files
    dplyr::left_join(
      readr::read_csv(here::here("data", paste0("FARS", x, "_Aux"), "ACC_AUX.csv")),
      readr::read_csv(here::here("data", paste0("FARS", x, "_Aux"), "PER_AUX.csv"))
    )
  }
)

fars_combined <-
  dplyr::left_join(
    fars,
    fars_aux
  )

readr::write_csv(fars_combined, here::here("output", "fars.csv"))
