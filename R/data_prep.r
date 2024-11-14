# utility functions involved with data prep

#' Finds filepath to specific river data
#'
#' \code{data_finder} takes a filepath and a river designator and returns the
#'     complete filepaths to specified river data.
#'
#' The function presumes data to be in xlsx format and the river designator,
#' within the name of the file, to be preceded by 8 digits (date).
#'
#' @param path Character string filepath to location of sonde data xlsx
#'     workbooks.
#'
#' @param river Character. Either "c" for Canning River or "s" for Swan
#'     River.
#'
#' @return If data meeting the required conditions exist, filepaths of data
#'     location. There should be two filepaths per date for upper and lower
#'     data runs.
#'
#' @examples
#' \dontrun{
#' data_finder(path = "Z:/DEC/MonitoringProgram/Data", river = "c")}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' For more details see  \url{https://dbca-wa.github.io/rivRmon/index.html}
#' {the rivRmon website}
#'
#' @importFrom stringr str_detect

data_finder <- function(path, river){
  excels <- list.files(path = path, pattern = ".xlsx$")
  excel_clean <- excels[stringr::str_detect(excels, "^\\d{8}")] #starts with digits only
  sorted <- if(river == "c"){
    out <- excel_clean[stringr::str_detect(excel_clean, "c")]
  } else {
    out <- excel_clean[!stringr::str_detect(excel_clean, "c")]
  }
  file.path(path, sorted)
}



#' Reads in sonde data from various model formats and creates tidy data
#'
#' \code{sonde_reader} takes a filepath to one xlsx workbook as identified
#'     by \code{\link{data_finder}} and determines:
#'     \itemize{
#'       \item determines whether data has two header rows
#'       \item cleans data
#'       \item returns a tidy data set
#'       }
#'
#' @param path Character string filepath to location of sonde data xlsx
#'     workbook (one only).
#'
#' @return A data frame containing:
#'     \itemize{
#'       \item site
#'       \item temperature
#'       \item dissolved oxygen mg/L
#'       \item salinity ppt
#'       \item depth
#'       \item chlorophyll a
#'       }
#'
#' @examples
#' \dontrun{
#' sonde_reader(path = "Z:/DEC/MonitoringProgram/Data/20191201cpoEXO1.xlsx")}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' For more details see  \url{https://dbca-wa.github.io/rivRmon/index.html}
#' {the rivRmon website}
#'
#' @importFrom readxl read_excel
#' @import dplyr
#' @import stringr
#' @importFrom tidyr spread
#' @importFrom unpivotr as_cells

sonde_reader <- function(path){
  # make sure of correct tab
  sheet <- readxl::excel_sheets(path)[stringr::str_sub(stringr::str_to_lower(readxl::excel_sheets(path)), 1, 1) == "e"]
  
  # read in and see what we have
  dat <- readxl::read_excel(path, sheet = sheet)
  
  # handle "YV" sondes with double header
  if(stringr::str_detect(dat[1,1], "D/M/Y|M/D/Y")){
    dat2 <- readxl::read_excel(path, sheet = sheet, col_names = FALSE)
    cells <- unpivotr::as_cells(dat2)
    col_headers <-
      cells %>%
      dplyr::filter(row <= 2, !is.na(chr)) %>%
      dplyr::select(row, col, header = chr) %>%
      tidyr::spread(row, header) %>%
      dplyr::mutate(`2` = ifelse(is.na(`2`), "_X", `2`)) %>%
      dplyr::mutate(header = ifelse(`2` != "_X", paste(`1`, `2`), `1`)) %>%
      dplyr::pull(header)
    dat3 <- readxl::read_excel(path, sheet = sheet, skip = 2, col_names = FALSE)
    names(dat3) <- tolower(col_headers)
    
    # standardise names
    search_for_these <- c("site codes", "sitenum", "site name", "site", "site code", "site names",
                          "temp c", "temp °c", "temp øc", "temp", "°c",
                          "odo mg/l", "do+ conc mg/l" , "do mg/l",
                          "sal ppt", "salinity ppt", "sal-ppt",
                          "depth meters", "depth m", "depth", "dep m",
                          "chl ug/l", "chlorophyll µg/l", "chlorophyll ug/l", "chlorophyll æg/l")
    replace_with_these <- c(rep("Site", 6), rep("°C", 5), rep("DO mg/L", 3),
                            rep("SAL-ppt", 3), rep("DEP m", 4), rep("Chl ug/L", 4))
    found <- match(colnames(dat3), search_for_these, nomatch = 0)
    colnames(dat3)[colnames(dat3) %in% search_for_these] <- replace_with_these[found]
    
    # return only required data for surfer
    dat4 <- dat3 %>%
      dplyr::select(Site, '°C', 'DO mg/L', 'SAL-ppt', 'DEP m', 'Chl ug/L')
    
    return(dat4)
    
  } else {
    
    # no double headers proceed with standardise names
    names(dat) <- tolower(names(dat))
    search_for_these <- c("site codes", "sitenum", "site name", "site", "site code", "site names",
                          "temp c", "temp °c", "temp øc", "temp", "°c",
                          "odo mg/l", "do+ conc mg/l" , "do mg/l",
                          "sal ppt", "salinity ppt", "sal-ppt",
                          "vpos m",
                          "chl ug/l", "chlorophyll µg/l", "chlorophyll ug/l", "chlorophyll æg/l")
    replace_with_these <- c(rep("Site", 6), rep("°C", 5), rep("DO mg/L", 3),
                            rep("SAL-ppt", 3), rep("DEP m", 1), rep("Chl ug/L", 4))
    found <- match(colnames(dat), search_for_these, nomatch = 0)
    colnames(dat)[colnames(dat) %in% search_for_these] <- replace_with_these[found]
    
    # return only required data for surfer
    dat4 <- dat %>%
      dplyr::select(Site, '°C', 'DO mg/L', 'SAL-ppt', 'DEP m', 'Chl ug/L')
    
    return(dat4)
  }
  
}

#' Reads in sonde data and formats specifically for use for plotting single metrics
#' 
#' \code{sonde_reader_metric} takes a filepath to one xlsx workbook as identified
#'    by \code{\link{data_finder}} and formats it for single metric plotting.
#'     
#' @param path Character string filepath to location of manually combined sonde 
#'    data xlsx workbook (one only). Looks for shhet tab beginning with the letter 
#'    'e'.
#'     
#' @return A data frame where the 'VPos' variable has been renamed to "Dep m" 
#'    to allow for consistency with legacy code.
#'    
#' @examples
#' \dontrun{
#' sonde_reader_metric(path = "Z:/DEC/MonitoringProgram/Data/20191201cpoEXO1.xlsx")}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' For more details see  \url{https://dbca-wa.github.io/rivRmon/index.html}
#' {the rivRmon website}
#' 
#' @import dplyr
#' @import stringr
#' @importFrom readxl read_excel excel_sheets
#' @importFrom lubridate as_date
sonde_reader_metric <- function(path){
  # make sure of correct tab
  sheet <- readxl::excel_sheets(path)[stringr::str_sub(stringr::str_to_lower(readxl::excel_sheets(path)), 1, 1) == "e"]
  
  # read in and see what we have
  dat <- readxl::read_excel(path, sheet = sheet) |>
    dplyr::rename_with(tolower) |>
    rename_with(~ gsub("vpos(.*)", "dep m", .x), starts_with("vp")) |> # we use 'vpos' but call it 'dep m'
    dplyr::mutate(date = lubridate::as_date(date))
  
  return(dat)
  
}

#' Creates correct colour palette for measured metric
#'
#' \code{surfer_cols} takes metric designator and returns a named charcater
#'     of colours.
#'
#' @param metric Character string, "sal" for Salinity, "do" for Dissolved
#'    Oxygen mg/L, "chl" for Chlorophyll a, or "temp" for Temperature.
#'
#' @return A named character vector where names relate to data break values and
#'    characters are colour hex codes.
#'
#' @examples
#'  \dontrun{
#'colours <- surfer_cols(metric = "chl")}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' For more details see  \url{https://dbca-wa.github.io/rivRmon/index.html}
#' {the rivRmon website}
surfer_cols <- function(metric){
  if(metric == "sal"){
    sal_brk <- c(as.character(seq(2, 40, 2)), "100")
    sal_cols <- c("#996633", "#916E3D", "#897648", "#817E53", "#79865D", "#718E68",
                  "#699673", "#619E7E", "#59A688", "#51AE93", "#49B69E", "#41BEA9",
                  "#39C6B3", "#31CEBE", "#29D6C9", "#21DED4", "#19E6DE", "#11EEE9",
                  "#09F6F4", "#00FFFF", "#00FFFF")
    names(sal_cols) <- sal_brk
    return(sal_cols)
  } else if(metric == "do"){
    do_mg_l_brk <- c(as.character(seq(1, 16, 1)), "100")
    do_mg_l_cols <- c("#FF0000", "#FF6600", "#FFCC00", "#DAC35D", "#B5BABA",
                      "#A4E3E3", "#52F1F1", "#29DFF8", "#00CCFF", "#61EDC7",
                      "#99FF99", "#99FF4D", "#99FF00", "#73D90C", "#4DB319",
                      "#278D26", "#006633")
    names(do_mg_l_cols) <- do_mg_l_brk
    return(do_mg_l_cols)
  } else if(metric == "chl"){
    chl_brk <- c(as.character(seq(20, 80, 20)), "120", "160", "200", "300", "400", "1000")
    # chlr_cols <- c("white","#d6f5d6","#99e699", "#47d147", "#248f24", "#145214",
    #                "#990000", "#ff0000")
    chlr_cols <- c("#e6ffff","#ebf9eb","#ccf2cc", "#a3e8a3", "#7ade7a", "#52d452",
                   "#2eb82e", "#990000", "#cc0000", "#ff0000")
    names(chlr_cols) <- chl_brk
    return(chlr_cols)
  } else {
    temp_brk <- c(as.character(seq(11, 32, 1)), "100")
    temp_cols <- c("#00FFFF", "#0CECFF", "#19D9FF", "#26C6FF", "#33B3FF",
                   "#3FA0FF", "#4C8DFF", "#5284FF", "#597AFF", "#6373F0",
                   "#6D6BE0", "#7764D0", "#825CC0", "#8C55B0", "#974DA0",
                   "#A14590", "#AC3D80", "#B63670", "#C02E60", "#CA2750",
                   "#D51F40", "#DF1830", "#EA1020")
    names(temp_cols) <- temp_brk
    return(temp_cols)
  }
}

#' Creates correct suffix for day of month
#'
#' \code{just_nums} takes a numeric value and returns a character equivalent
#'     with the correct suffix, e.g. 'th', 'nd' etc.
#'
#' @param n numeric value for day of month.
#'
#' @return A character value with correct suffix.
#'
#' @examples
#'  \dontrun{
#'just_nums(n = 2)}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' For more details see  \url{https://dbca-wa.github.io/rivRmon/index.html}
#' {the rivRmon website}
#'
#' @importFrom dplyr case_when
just_nums <- function(n){
  
  suff <- case_when(n %in% c(11,12,13) ~ "th",
                    n %% 10 == 1 ~ 'st',
                    n %% 10 == 2 ~ 'nd',
                    n %% 10 == 3 ~'rd',
                    TRUE ~ "th")
  paste0(n, suff)
}

#' Creates file paths to raw phytoplankton xlsx spreadsheets
#'
#' \code{phyto_finder} takes a file path to raw phytoplankton xlsx spreadsheets
#'     and returns a full filepath for use in other functions.
#'
#' @details This is a helper function to set up correct file paths to the
#'     appropriate data xlsx spreadsheets.
#'
#' @param pathin a character filepath to the location of the raw xlsx
#'     spreadsheets
#'
#' @examples
#' \dontrun{
#' phyto_finder(pathin = "C:/path/to/raw_data")
#' }
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' For more details see  \url{https://dbca-wa.github.io/rivRmon/index.html}
#' {the rivRmon website}
#'
#' @importFrom stringr str_detect
phyto_finder <- function(pathin){
  excels <- list.files(path = pathin, pattern = ".xlsx$")
  excel_clean <- excels[stringr::str_detect(excels, "^\\d{6}")] #starts with digits only
  file.path(pathin, excel_clean)
}

