# Function to parse interim Chem Centre LIMS data

#' Creates summary LIMs data for interim reporting
#'
#' \code{lims_parsR} takes a file path to raw LIMS xlsx spreadsheets
#'     and creates summary data.
#'
#' @details It takes a file path to 3 raw CSV files comprising of a week's 
#'     worth of sampling as output from the Chem Centre. It then outputs a 
#'     summary CSV data file, categorising chlorophyll a values into low, medium 
#'     and high. "Low" is < 4ug, "medium" is 4 to 10ug and higher represents the 
#'     "high" category. Summary output is combined for both rivers. Output has 
#'     first day's date of sampling week and "interim_pigment_result" in the name.
#'
#' @param pathin a character filepath to the location of the week's raw LIMS 
#'     data. Expects 3 CSV files, 2 for the Swan and one for the Canning. Data 
#'     summary is written to this location.
#'
#' @param skip numeric number of lines to skip at beginning of LIMS data ingest.
#'     Defaults to 8 which suits current LIMS format, change only if required.
#'
#' @examples
#' \dontrun{
#' lims_parsR(pathin = "C:/path/to/raw_data", skip = 8)
#' }
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' For more details see  \url{https://Bartesto.github.io/rivRmon/index.html}
#' {the rivRmon website}
#'
#' @importFrom readr read_csv write_csv
#' @importFrom tibble tibble
#' @importFrom hms as_hms
#' @importFrom lubridate dmy
#' @import dplyr
#' 
#' @export
lims_parsR <- function(path, skip = 8){
  # find names for full paths to a weeks worth of data sets
  data_todo <- list.files(path = path, pattern = ".csv",
                          full.names = TRUE)
  
  # check for 3 csv
  if(length(data_todo) == 3){
    #get closest prior date to current date
    data_todo <- data_todo
  } else {
    stop("Not the correct number of input csv's")
  }
  
  # set up site df's in correct upriver order
  s_names <- c("BLA", "ARM", "HEA", "NAR", "NIL", "STJ", "MAY", "RON", "KIN", 
               "SUC", "WMP", "MSB")
  c_names <- c("SCB2", "SAL", "SHELL", "RIV", "CASMID", "KEN", "BAC", "NIC", "ELL")
  # df_swan <- dplyr::data_frame(site = s_names)
  # df_cann <- dplyr::data_frame(site = c_names)
  
  df_swan <- tibble::tibble(site = s_names)
  df_cann <- tibble::tibble(site = c_names)
  
  # create summary of all sites for week
  df_summary <- tibble::tibble()
  
  for(i in seq_along(data_todo)){
    # get column headers
    col_names <- names(readr::read_csv(data_todo[i], n_max = 0))
    # read in data, add back headers and skip metadata type entries
    df <- readr::read_csv(data_todo[i], col_names = col_names, skip = skip)
    # standard names - easier
    names(df) <- tolower(col_names)
    # munge and mutate
    df_grab <- df %>%
      dplyr::filter(`collection method` == "ID") %>%
      dplyr::select(`site reference`, `date collected`, 
                    `time`, contains("chlorophyll a" )) #can have more than 1!
    df_out <- df_grab[colSums(!is.na(df_grab)) > 0] %>%
      dplyr::rename(site = `site reference`,
                    date = `date collected`,
                    chla_mg = starts_with("chlorophyll")) %>%
      dplyr::mutate(chla_ug = as.numeric(chla_mg) * 1000,
                    date = lubridate::dmy(date),
                    time = hms::as_hms(time),
                    category = case_when(
                      chla_ug <= 3.999 ~ "LOW",
                      chla_ug <= 10 ~ "MEDIUM",
                      TRUE ~ "HIGH"))
    # add to summary data
    df_summary <- dplyr::bind_rows(df_summary, df_out)
  }
  
  # match to separate river df's to maintain correct order, join back together
  a <- dplyr::left_join(df_swan, df_summary, by = "site") %>%
    dplyr::mutate(river = "swan")
  b <- dplyr::left_join(df_cann, df_summary, by = "site") %>%
    dplyr::mutate(river = "cann")
  c <- dplyr::bind_rows(a, b)
  
  # create appropriate name and save results
  out_name <- paste0(path, "/", 
                     dplyr::pull(c[1, 2]), "_interim_pigment_results.csv")
  readr::write_csv(c, out_name)
}