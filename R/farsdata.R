#' Read the FARS data.
#'
#' Read the file and convert it to a tibble.
#'
#' @param filename the filename of the file as a string.
#' @return the contents of the file as a tibble.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Construct the filename for the year.
#'
#' Construct the filename of the accidents data
#' file in csv.bz2 format for the given year.
#'
#' @param year the year for which to construct the filename.
#'    Can be an integer or a string that can be converted to
#'    an integer.
#' @return the constructed filename as a string.
#'
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read the data for a list of years
#'
#' Successively read the accidents files for each
#' year provided and collects the tibbles in a list.
#' Each tibble contains only year and month columns.
#'
#' @param years the list or vector of integer years to read.
#' @return a list with a tibble for each year containing columns
#'   MONTH and year. If the file does not exist or does not have
#'   the correct data, then a warning is emitted and NULL returned.
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select("MONTH", year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Generate monthly accidents summary table.
#'
#' Read the data for some years and generates
#' a table containing the monthly totals for
#' each year.
#'
#' @param years the list of integer years to read.
#' @return a tibble containing the monthly totals for each year.
#'
#' @importFrom dplyr bind_rows
#' @importFrom magrittr %>%
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom dplyr group_by
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Plot state accident data for year.
#'
#' Plot a map of a state with points for each
#' case of the given year.
#'
#' @param state.num the numerical identifier of the state to plot.
#' @param year the year to process and display.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}

