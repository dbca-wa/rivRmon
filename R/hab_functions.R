# Functions to create HAB tables

#' Creates summary HAB data for conditionally formatted table export
#'
#' \code{hab_groupR} takes a file path to raw PEU xlsx spreadsheets
#'     and creates summary data for use HAB reporting tables.
#'
#' @details This is a precursor function to be run prior to using
#'     (\code{\link{hab_tablR}}). It takes a file path to raw xlsx
#'     phytoplankton data, i.e. PEU data, creates cummulative counts on specific 
#'     reportable phytoplankton groups and outputs a summary csv file. The 
#'     groups are determined from an input file of management species and 
#'     trigger levels.
#'
#'     Summary csv files are exported to a `HAB_tables/` directory and are named
#'     with the date, estuary and the words "HAB_data". If the directory doesn't
#'     exist, it is created.
#'
#' @param pathin a character filepath to the location of the raw phytoplankton
#'     xlsx spreadsheets i.e. the PEU data
#'
#' @param pathout a character filepath to the location of the desired directory
#'     for the summary csv files, suggest same as `pathin`
#'     
#' @param mngt_triggers a character filepath and name of the 
#'     `mngt_response_triggers_2020.csv` data file that indicates group names, 
#'     species names and trigger thresholds for management action. See package
#'     example dataset `mngt_response_triggers` for example of format required.
#'     
#' @param skip numeric number of lines to skip at beginning of PEU data ingest.
#'     Defaults to 5 which suits current PEU format, change only if required
#'     
#' @param shell logical. Include Canning River site "Shell". Defaults to FALSE, 
#'     change if required
#'
#' @examples
#' \dontrun{
#' phyto_groupR(pathin = "C:/path/to/raw_data", pathout = "C:/path/for_export",
#' mngt_triggers = "./mngt_response_triggers_2020.csv", skip = 5, shell = FALSE)
#' }
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' For more details see  \url{https://Bartesto.github.io/rivRmon/index.html}
#' {the rivRmon website}
#'
#' @import stringr
#' @import readxl
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom lubridate ymd
#' @importFrom tidyr drop_na pivot_wider
#' @importFrom readr write_csv
#'
#' @export
hab_groupR <- function (pathin, pathout, mngt_triggers, skip = 5, shell = FALSE){
  
  locations <- phyto_finder(pathin)
  # make folder for outputs
  hab_folder <- file.path(pathout, "HAB_tables") ##
  if (!file.exists(hab_folder)) {
    dir.create(hab_folder)
  } ##
  
  t_table <- readr::read_csv(mngt_triggers)
  
  for (i in seq_along(locations)) {
    loc <- locations[i]
    sheet <- readxl::excel_sheets(loc)[stringr::str_sub(stringr::str_to_lower(readxl::excel_sheets(loc)), 1, 1) != "e"]
    sheet1 <- sheet[sheet != "Sheet1"] # in case of blank work sheet
    dat <- readxl::read_excel(loc, sheet = sheet1, skip = skip)
    names(dat) <- tolower(names(dat))
    
    #stuff to get leading date 6 or 8 digit
    loc_splt <- stringr::str_split(loc, pattern = "/")
    loc_splt_1 <- stringr::str_split(loc_splt[[1]][length(loc_splt[[1]])], "_")
    samp_date <- lubridate::ymd(loc_splt_1[[1]][1])
    project <- dat[[1, "project"]]
    outpath_hab <- paste0(hab_folder, "/") ##
    
    # wiggy stuff here to work out river and supply names
    if(project == "SG-E-CANEST" & shell == FALSE){
      site_df <- tibble(site = c("SCB2", "SAL", "RIV", "CASMID", "KEN", "BAC", 
                                 "NIC",  "ELL"))
    } else if(project == "SG-E-CANEST" & shell == TRUE){
      site_df <- tibble(site = c("SCB2", "SAL", "SHELL", "RIV", "CASMID", "KEN", 
                                 "BAC", "NIC", "ELL"))
    } else if(project == "SG-E-SWANEST"){
      site_df <- tibble(site = c("BLA",	"ARM", "HEA",	"NAR", "NIL", "STJ", 
                                 "MAY", "RON", "KIN", "SUC", "WMP", "MSB"))
    } else {
      stop("I can't determine which river from the project in the raw data")
    }
    
    table_names_df <- t_table %>%
      dplyr::mutate(table_name = forcats::fct_inorder(table_name)) %>%
      dplyr::select(table_name) %>%
      dplyr::distinct(table_name) %>%
      dplyr::mutate(table_ord = 1:length(table_name))
    
    hab_dat2 <- dat %>% dplyr::select(siterefname, 
                                      species_name, harmfulspecies,
                                      species_density_cells_per_ml) %>% 
      dplyr::rename(site = siterefname, 
                    species = species_name, harm = harmfulspecies, 
                    density = species_density_cells_per_ml) %>%
      dplyr::mutate(species = tolower(gsub("\\.", "_", make.names(species, unique = FALSE))),
                    date = samp_date,
                    site = site) %>%
      dplyr::left_join(t_table, by = c("species" = "spp_name")) %>%
      tidyr::drop_na()
    
    hab_dat3 <- hab_dat2 %>%
      dplyr::right_join(site_df, by = "site") %>%
      dplyr::group_by(site, table_ord, table_name, surveillance, notification, date) %>%
      dplyr::summarise(density = sum(density, na.rm = TRUE)) %>%
      dplyr::ungroup()
    
    
    # add in version of mngt triggers
    mngt <- sapply(stringr::str_split(mngt_triggers, "/"), tail, 1)
    
    hab_dat4 <- hab_dat3 %>%
      dplyr::mutate(site = factor(site, levels = site_df[["site"]])) %>%
      dplyr::arrange(site) %>%
      tidyr::pivot_wider(names_from = site, values_from = density) %>%
      dplyr::full_join(table_names_df, by = "table_name") %>%
      dplyr::arrange(table_ord.y) %>%
      dplyr::ungroup() %>%
      dplyr::select(-table_ord.x, -table_ord.y, -surveillance, -notification, -date) %>%
      dplyr::mutate(mngt = mngt) %>%
      tidyr::drop_na(table_name)
    
    readr::write_csv(hab_dat4, paste0(outpath_hab, samp_date, "_", project, "_HAB_data.csv"))
  }
}

#' Creates conditionally formatted table as a pptx file from summary HAB data
#'
#' \code{hab_tablR} takes a file path to summary HAB data and a date and produces
#'     "present" and "prior sample" table of phytoplankton counts per site.
#'
#' @details This function can only be run on data created by running the
#'     (\code{\link{hab_groupR}}) function. It will read in all the summary
#'     data then based on the provided date, produce a conditionally formatted 
#'     table for that date. Values are coloured according to "investigate" or 
#'     "notification" count level for river sites for specific species. The prior
#'     sample's data is reported so comparisons can be made.
#'
#'     Data is exported as pptx file to the `HAB_tables/` directory. It can be 
#'     dragged and dropped into a PowerPoint template with some minor resizing.
#'
#' @param hab_tables a character file path to the location of the `HAB_tables/`
#'     directory.
#'
#' @param date a character representation of the date chosen for the "present"
#'     sampling week in the format "yyyymmdd".
#'     
#' @param mngt_triggers a character filepath and name of the 
#'     `mngt_response_triggers_YYYY.csv` data file that indicates group names, 
#'     species names and trigger thresholds for management action. See package
#'     example dataset `mngt_response_triggers` for example of format required.
#'
#' @examples
#' \dontrun{
#' hab_tablR(summary = "C:/path/to/HAB_tables", date = "20190910",
#' mngt_triggers = "./mngt_response_triggers_2020.csv")
#' }
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' For more details see  \url{https://Bartesto.github.io/rivRmon/index.html}
#' {the rivRmon website}
#'
#' @importFrom readr read_csv write_csv
#' @importFrom tidyr gather
#' @importFrom stringr str_split str_detect
#' @importFrom lubridate ymd
#' @importFrom rlang call2
#' @importFrom tibble tibble
#' @importFrom ggpubr ggarrange
#' @import dplyr
#' @import ggplot2
#' @import huxtable 
#'
#' @export
hab_tablR <- function(hab_tables, date, mngt_triggers){
  files <- file.path(hab_tables, list.files(hab_tables, "HAB_data.csv"))
  #dynamic length due to unknown file path
  d_length <- length(stringr::str_split(files[1], "/")[[1]])
  est <- substr(sapply(stringr::str_split(files[1], "/"), "[[", d_length), 12, 
                23)
  
  dat <- data.frame()
  for(i in seq_along(files)){
    dat1 <- readr::read_csv(files[i])
    dat1$date <- lubridate::ymd(stringr::str_split(files[i], "/")[[1]][d_length])
    dat <- dplyr::bind_rows(dat, dat1)
  }
  
  # unique dates in summaries
  udates <-unique(dat$date)
  
  # user entered current date
  current <- lubridate::ymd(date)
  
  # test if current date in data
  if(current %in% udates){
    #create other dates vector of less than current date
    odates <- udates[udates < current]
  } else {
    stop("Your date is not in the data")
  }
  
  # test if there is a prior date and select it
  if(sum(odates < current) > 0){
    #get closest prior date to current date
    closest_index <- which(abs(odates - current) == min(abs(odates - current)))
    last <- odates[closest_index]
  } else {
    stop("No data in HAB data for prior period")
  }
  
  # intended mnt response table
  mngt_curr <- sapply(stringr::str_split(mngt_triggers, "/"), tail, 1)
  
  # current week
  now <- dat %>%
    dplyr::filter(date == current)
  
  # check 'now' data has been created by intended mngt triggers
  if(now[["mngt"]][1] == mngt_curr){
    # all good remove trigger info
    now <- dplyr::select(now, -mngt)
  } else {
    stop("Current week HAB data not created with current trigger info")
  }
  
  # last week
  prior <- dat %>%
    dplyr::filter(date == last)
  
  # check 'prior' data has been created by intended mngt triggers
  if(prior[["mngt"]][1] == mngt_curr){
    # all good remove trigger info
    prior <- dplyr::select(prior, -mngt)
  } else {
    stop("Prior week HAB data not created with current trigger info")
  }
  
  # set up empty tibble to add alternating columns to
  weeks_df <- tibble(table_name = now[,1])
  
  for(i in 2:length(now)-1){
    df1 <- dplyr::bind_cols(prior[i], now[i])
    weeks_df <- dplyr::bind_cols(weeks_df, df1)
  }
  
  # add in upper and lower breaks for values
  t_table_fn <- readr::read_csv(mngt_triggers) %>%
    dplyr::mutate(table_name = fct_inorder(table_name)) %>%
    dplyr::group_by(table_name) %>%
    dplyr::summarise(surveillance = mean(surveillance),
                     notification = mean(notification)) %>%
    dplyr::mutate(b_l = surveillance,
                  b_u = notification)
  
  # clean and setup initial huxtable object
  hux_df <- weeks_df %>%
    dplyr::select(-table_name1, -table_name2) %>%
    huxtable::as_huxtable(add_colnames = TRUE) %>% 
    huxtable::set_bold(1, everywhere, TRUE)
  
  # loop for text colouring to trigger levels
  for(i in 2:length(hux_df$table_name)){
    warn_colours = c(NA, "orange", "red")
    r <- i
    b <- c(t_table_fn[[i - 1, 4]], t_table_fn[[i - 1, 5]])
    hux_df <- eval(rlang::call2("map_text_color", ht = hux_df, row = r,
                                col = 2 :length(hux_df), 
                                rlang::call2("by_ranges", breaks = b, 
                                             values = warn_colours)))
  }
  
  # grey out prior data week
  hux_df2 <- hux_df %>%
    huxtable::set_background_color(row = 2:length(hux_df[[1]]), 
                                   col =  evens, value = "#D9D9D9") %>% 
    huxtable::set_all_borders(1)
  
  # make index to merge columns by
  merge_ind <- tibble::tibble(f = seq(from = 2, length.out = length(now) - 2,
                                      by = 2), 
                              t = seq(from = 3, length.out = length(now) - 2,
                                      by = 2))
  
  # loop for column merging
  for(i in 1:length(merge_ind[[1]])){
    hux_df2 <- eval(rlang::call2("merge_cells", ht = hux_df2, row = 1, 
                                 col = c(merge_ind[["f"]][i],
                                         merge_ind[["t"]][i])))
  }
  
  # tidy number format from sci and text size for all
  huxtable::number_format(hux_df2)[,-1] <- NA
  huxtable::font_size(hux_df2) <- 10
  
  # make appropriate name
  out_name <- file.path(hab_tables, paste0("HAB_report_week_", est, "_", 
                                           current, ".pptx"))
  # save out as pptx
  huxtable::quick_pptx(hux_df2, file = out_name)
}