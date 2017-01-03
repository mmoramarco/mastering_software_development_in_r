#' 
#' @description 
#' Generates a data frame from a file. If the file does not exist
#' the function will stop with an error message.
#'
#' @param filename A string.
#' @return This function returns a data frame from reading \code{filename}.
#' @examples
#' fars_read("accident_2014.csv.bz2")
#' fars_read("accident_2015.csv.bz2")
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
#' 
#' @description 
#' Standardizes a filename based on the year. The value of \code{year}
#' must be compatible with as.integer otherwise an error will result.
#'
#' @param year A number representing the year
#' @return This function returns a string based on \code{year}.
#' @examples
#' make_filename(2014)
#' make_filename(2015)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}
#' 
#' @description 
#' Reads files into a list of data.frames with two columns, MONTH and year, 
#' over the vector of years input. All values in the vector years need to be
#' convertable into an integer otherwise the make_filename function wll error.
#'
#' @param years A vector of numbers representing years for analysis
#' @return This function returns a list of data.frames with the 
#'    length of the vector \code{years} and two columns: MONTH and year.
#' @examples
#' fars_read_years(c(2014,2015))
#' fars_read_years(2013:2015)
#' @importFrom dplyr mutate
#' @importFrom dplyr select
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
#' 
#' @description 
#' Summarizes the count of accidents by month and year for the input
#' vector \code{years}.
#'
#' @param years A vector of numbers representing years for analysis
#' @return This function returns a data.frame with months as rows
#'    and years as columns. Each combination contains the count of
#'    accidents.
#' @examples
#' fars_summarize_years(2012:2014)
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}
#' 
#' @description 
#' This function plots the number of accidents for a specified state.
#' 
#' This function returns an error if either the \code{state.num} or 
#' \code{year] do not exist in the dataset
#'
#' @param state.num A number representing a state in the FARS data
#' @param year A number representing the year for analysis
#' @return 
#' @examples
#' fars_map_state('01',2014)
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
