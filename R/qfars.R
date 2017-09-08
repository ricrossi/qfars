#' Check the existenence of a file and read it
#'
#' This function takes as only argument a filename and first check if the inputted filename exists.
#'    If the file does not exist in current working directory an error message is given, otherwise the file is
#'    read as a dataframe
#'
#' @param filename The name of the file to be read. The filename should be built with the
#'    "make_filename" function.
#'
#' @return This function returns a tibble (dataframe). If the filename provided as argument is wrong or the file
#'    is not present in current working directory, the function returns an error message.
#'
#' @importFrom readr read_csv
#'
#' @examples
#' \dontrun{data2013 <- fars_read("accident_2013.csv.bz2")}
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


#' Build the filename for a specific year
#'
#' This function takes as argument a four digit number indicating a year to build the complete
#'    filename for that year, according to FARS standard naming of datasets
#'
#' @param year The year, expressed in four digit, for which the filename string needs to be returned
#'
#' @return This function returns a character vector which is the filename for the specified year
#'
#' @examples
#' \dontrun{fars_read(2013)}
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Iteratively read months and years from different filenames
#'
#' This function is used to iteratively read files relative to fatalities in a series
#' of initially selected years, i.e. from different files referring to different years.
#'
#' @param years A list of years for which the observation needs to be extracted
#'
#' @return This function returns a list of data frames, one for each year of the argument list and
#'    containing the months and year for each observation. If one of the specified years is not present
#'    in the dataset an error message
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{multi_years <- fars_read_years(list(2013, 2014, 2015))}
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize accidents per year and month
#'
#' This function creates a summary table displaying the number of accidents for each month and year
#'
#' @param years A list of years for the user wants to see the summary, or a single year expressed
#'    as a four digit numeral
#'
#' @return This function returns a table with the months aggregates of accidents (in rows)
#'    for each year (in columns)
#'
#' @note Please note that if one of the specified years is not present in the dataset an error message
#'    is returned specifying which year is invalid (not found).
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{summary1 <- fars_summarize_years(2014)}
#' \dontrun{summary2 <- fars_summarize_years(list(2013, 2014))}
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Query accidents for a specific State and year
#'
#' This function serves to extract fars accidents for a specific state and year and, if present,
#'    print a map with accidents
#'
#' @param state.num The State ID number (from 1 to 56) for the State to be queried
#' @param years A four digit number of the year the accidents need to be extracted for
#'
#' @return This function prints a map of the specified State with dots for each documented accident
#'
#' @note Error messages are displayed in case an invalid State id number is used as an argument or
#'    it's not present and in case no accidents are documented for the specified State and year
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(33, 2014)}
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, data$STATE == state.num)
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
