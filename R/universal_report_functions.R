#' Helper function for extracting legend from a plot
#'
#' \code{get_legend} takes a ggplot object and returns the legend grob
#'
#' @param myggplot a ggplot object that contains a legend
#'
#' @return a legend grob
#'
#' @examples
#' \dontrun{
#' get_legend(myggplot)}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' For more details see  \url{https://dbca-wa.github.io/rivRmon/index.html}
#' {the rivRmon website}
#'
#' @import ggplot2

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#' Helper function to calculate sensible y axis limits from the data
#'
#' \code{dynamic_ylim} takes the data for a variable and calculates the maximum
#'     value of the background and present period plotting variables, finds the
#'     maximum of those values and adds 10%. This bcomes the upper y limit.
#'     Lower y limit is zero. Is highly very specific to data structure created
#'     when processing the annual report data.
#'
#' @param x the data.
#'
#' @return a numeric vector of length 2, first value zero, second the calculated
#'     y limit. Will be passed to ylim argument in a ggplot call.
#'
#' @examples
#' \dontrun{
#' dynamic_ylim(data)}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' For more details see  \url{https://dbca-wa.github.io/rivRmon/index.html}
#' {the rivRmon website}
#'
#' @import dplyr
#'
#' @export

dynamic_ylim <- function(x){
  val1 <- dplyr::ungroup(x) %>% dplyr::filter(rep_per == "background") %>%
    dplyr::summarise(umax = max(upper))
  val2 <- dplyr::ungroup(x) %>% dplyr::filter(rep_per == "present") %>%
    dplyr::summarise(mmax = max(median))
  dyn_ylim <- c(0, max(c(val1[[1]], val2[[1]])) * 1.1)
  return(dyn_ylim)
}

#' Function to read and format water quality data obtained from the WIR water
#' portal for use in annual reporting for the Canning River
#'
#' \code{canning_WIN_report_data} reads in all xlsx downloaded files from WIR
#'     and based on a reporting year, formats the data for use in generating the
#'     annual report.
#'
#' @details Original source data must be downloaded from
#'     \url{http://wir.water.wa.gov.au/Pages/Water-Information-Reporting.aspx}
#'     using the keyword "SG-E-CANEST" and requesting "Water Quality (discrete)
#'     for Site(s) cross-tabulated". These datasets are then read in and
#'     reformatted for ease of use in the annual report.
#'
#' @param inpath character filepath to the downloaded data xlsx files.
#'
#' @param reportingYear 4 digit numerical representing current reporting year.
#'
#' @param outpath character filepath to desired export location.
#'
#' @return a csv export named SG-E-CANEST_annual_report_data_for_YYYY.
#'
#' @examples
#' \dontrun{
#' canning_WIN_report_data(inpath, reportYear = 2019, outpath)}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' For more details see  \url{https://dbca-wa.github.io/rivRmon/index.html}
#' {the rivRmon website}
#'
#' @import tidyverse
#' @importFrom  lubridate month
#' @import readxl
#' @import janitor
#'
#' @export

canning_WIN_report_data <- function(inpath, reportingYear = 2019, outpath){
  files <- list.files(path = inpath, pattern = ".xlsx",
                      full.names = TRUE)

  #determine river from data
  riv <- ifelse(str_detect(files[1], "SWANEST"), "/SG-E-SWANEST_", "/SG-E-CANEST_")

  #assumes first 24 names in double headed spreadsheet are same for each import
  #dynamically assigns text vals to all columns after first 24 - function
  #conv_l_g handles conversion to numeric later
  t_names <- names(suppressWarnings(readxl::read_excel(files[1])))
  names24 <- names(suppressWarnings(readxl::read_excel(files[1], skip = 1)))[1:24] #first 24 are consistent
  new_names <- c(names24, t_names[25:length(t_names)])
  ctypes <- c(rep("guess", 3), "date", rep("guess", 20), rep("text", length(t_names) - 24))
  d1 <- suppressWarnings(readxl::read_excel(files[1], skip = 2, col_names = new_names,
                                            col_types = ctypes, .name_repair = ~ janitor::make_clean_names))

  #these are the fields to import Note these are different depending on river
  fields <- c("collection_method", "sample_type", "collect_date",
              "project_site_ref",
              "c_sol_org_doc_doc_as_npoc_mg_l", "chlorophyll_a_by_vol_mg_l",
              "n_sum_sol_org_don_mg_l", "n_sum_sol_ox_n_ox_n_ton_mg_l",
              "n_tot_tn_p_tn_mg_l", "nh3_n_nh4_n_sol_mg_l", "o_do_in_situ_mg_l",
              "p_tot_tp_p_tp_mg_l", "po4_p_sol_react_srp_frp_mg_l",
              "salinity_mg_l", "secchi_depth_m", "si_o2_si_sol_react_mg_l",
              "temperature_in_situ_deg_c", "p_h_no_units")

  #function to convert less than greater than symbols to approved numeric values
  conv_l_or_g <- function(x){
    ifelse(str_detect(x, "^<"), as.numeric(sapply(str_split(x, "<"),
                                                  function(x) x[2]))/2,
           ifelse(str_detect(x, "^>"), as.numeric(sapply(str_split(x, ">"),
                                                         function(x) x[2])),
                  as.numeric(x)))
  }

  #dynamic background data date generator
  report_start <- paste0(reportingYear - 6, "-06-01")
  report_fin <- paste0(reportingYear - 1, "-05-31")

  #converting text to numeric and handling less and greater thans
  tidy_all <- suppressWarnings(d1 %>%
                                 select(fields) %>%
                                 mutate(c_sol_org_doc_doc_as_npoc_mg_l = conv_l_or_g(c_sol_org_doc_doc_as_npoc_mg_l),
                                        chlorophyll_a_by_vol_mg_l = conv_l_or_g(chlorophyll_a_by_vol_mg_l),
                                        n_sum_sol_org_don_mg_l = conv_l_or_g(n_sum_sol_org_don_mg_l),
                                        n_sum_sol_ox_n_ox_n_ton_mg_l = conv_l_or_g(n_sum_sol_ox_n_ox_n_ton_mg_l),
                                        n_tot_tn_p_tn_mg_l = conv_l_or_g(n_tot_tn_p_tn_mg_l),
                                        nh3_n_nh4_n_sol_mg_l = conv_l_or_g(nh3_n_nh4_n_sol_mg_l),
                                        o_do_in_situ_mg_l = conv_l_or_g(o_do_in_situ_mg_l),
                                        p_tot_tp_p_tp_mg_l = conv_l_or_g(p_tot_tp_p_tp_mg_l),
                                        po4_p_sol_react_srp_frp_mg_l = conv_l_or_g(po4_p_sol_react_srp_frp_mg_l),
                                        salinity_mg_l = conv_l_or_g(salinity_mg_l),
                                        secchi_depth_m = conv_l_or_g(secchi_depth_m),
                                        si_o2_si_sol_react_mg_l = conv_l_or_g(si_o2_si_sol_react_mg_l),
                                        temperature_in_situ_deg_c = conv_l_or_g(temperature_in_situ_deg_c),
                                        p_h_no_units = conv_l_or_g(p_h_no_units)))

  #loop to get all downloaded WIN data
  if(length(files) > 1){
    for(i in seq_along(files)[-1]){
      t_names1 <- suppressWarnings(names(readxl::read_excel(files[i])))
      new_names1 <- c(names24, t_names1[25:length(t_names1)])
      ctypes1 <- c(rep("guess", 3), "date", rep("guess", 20), rep("text", length(t_names1) - 24))
      d <- suppressWarnings(readxl::read_excel(files[i], skip = 2, col_names = new_names1,
                                               col_types = ctypes1, .name_repair = ~ janitor::make_clean_names))
      t_d <- suppressWarnings(d %>%
                                select(fields) %>%
                                mutate(c_sol_org_doc_doc_as_npoc_mg_l = conv_l_or_g(c_sol_org_doc_doc_as_npoc_mg_l),
                                       chlorophyll_a_by_vol_mg_l = conv_l_or_g(chlorophyll_a_by_vol_mg_l),
                                       n_sum_sol_org_don_mg_l = conv_l_or_g(n_sum_sol_org_don_mg_l),
                                       n_sum_sol_ox_n_ox_n_ton_mg_l = conv_l_or_g(n_sum_sol_ox_n_ox_n_ton_mg_l),
                                       n_tot_tn_p_tn_mg_l = conv_l_or_g(n_tot_tn_p_tn_mg_l),
                                       nh3_n_nh4_n_sol_mg_l = conv_l_or_g(nh3_n_nh4_n_sol_mg_l),
                                       o_do_in_situ_mg_l = conv_l_or_g(o_do_in_situ_mg_l),
                                       p_tot_tp_p_tp_mg_l = conv_l_or_g(p_tot_tp_p_tp_mg_l),
                                       po4_p_sol_react_srp_frp_mg_l = conv_l_or_g(po4_p_sol_react_srp_frp_mg_l),
                                       salinity_mg_l = conv_l_or_g(salinity_mg_l),
                                       secchi_depth_m = conv_l_or_g(secchi_depth_m),
                                       si_o2_si_sol_react_mg_l = conv_l_or_g(si_o2_si_sol_react_mg_l),
                                       temperature_in_situ_deg_c = conv_l_or_g(temperature_in_situ_deg_c),
                                       p_h_no_units = conv_l_or_g(p_h_no_units)))
      tidy_all <- dplyr::bind_rows(tidy_all, t_d)
    }
  } else {
    tidy_all <- tidy_all
  }

  #add reporting zones, plot order for months and key for report period
  tidy_all2 <- tidy_all %>%
    filter(project_site_ref != "SCB") %>%
    filter(collect_date >= report_start) %>%
    mutate(emz = case_when(project_site_ref == "SCB2" | project_site_ref == "SAL" | project_site_ref == "RIV" | project_site_ref == "CASMID" ~ "estuary",
                           project_site_ref == "KEN" | project_site_ref == "BAC" | project_site_ref == "NIC" | project_site_ref == "ELL" ~ "river",
                           TRUE ~ "nrz")) %>%
    mutate(mth = lubridate::month(collect_date),
           pord = case_when(mth == 1 ~ 8,
                            mth == 2 ~ 9,
                            mth == 3 ~ 10,
                            mth == 4 ~ 11,
                            mth == 5 ~ 12,
                            mth == 6 ~ 1,
                            mth == 7 ~ 2,
                            mth == 8 ~ 3,
                            mth == 9 ~ 4,
                            mth == 10 ~ 5,
                            mth == 11 ~ 6,
                            mth == 12 ~ 7),
           rep_per = ifelse(collect_date < report_fin, "background", "present"))

  #write out dataset
  write_csv(tidy_all2, path = paste0(outpath, riv, "annual_report_data_for_", reportingYear,".csv"))
}


#' Function to read and format water quality data obtained from the WIR water
#' portal for use in annual reporting for the Swan River
#'
#' \code{swan_WIN_report_data} reads in all xlsx downloaded files from WIR
#'     and based on a reporting year, formats the data for use in generating the
#'     annual report.
#'
#' @details Original source data must be downloaded from
#'     \url{http://wir.water.wa.gov.au/Pages/Water-Information-Reporting.aspx}
#'     using the keyword "SG-E-SWANEST" and requesting "Water Quality (discrete)
#'     for Site(s) cross-tabulated". These datasets are then read in and
#'     reformatted for ease of use in the annual report.
#'
#' @param inpath character filepath to the downloaded data xlsx files.
#'
#' @param reportingYear 4 digit numerical representing current reporting year.
#'
#' @param outpath character filepath to desired export location.
#'
#' @return a csv export named SG-E-SWANEST_annual_report_data_for_YYYY.
#'
#' @examples
#' \dontrun{
#' swan_WIN_report_data(inpath, reportYear = 2019, outpath)}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' For more details see  \url{https://dbca-wa.github.io/rivRmon/index.html}
#' {the rivRmon website}
#'
#' @import tidyverse
#' @importFrom lubridate month
#' @import readxl
#' @import janitor
#'
#' @export

swan_WIN_report_data <- function(inpath, reportingYear = 2019, outpath){
  files <- list.files(path = inpath, pattern = ".xlsx",
                      full.names = TRUE)

  #determine river from data
  riv <- ifelse(str_detect(files[1], "SWANEST"), "/SG-E-SWANEST_", "/SG-E-CANEST_")

  #assumes first 24 names in double headed spreadsheet are same for each import
  #dynamically assigns text vals to all columns after first 24 - function
  #conv_l_g handles conversion to numeric later
  t_names <- names(suppressWarnings(readxl::read_excel(files[1])))
  names24 <- names(suppressWarnings(readxl::read_excel(files[1], skip = 1)))[1:24] #first 24 are consistent
  new_names <- c(names24, t_names[25:length(t_names)])
  ctypes <- c(rep("guess", 3), "date", rep("guess", 20), rep("text", length(t_names) - 24))
  d1 <- suppressWarnings(readxl::read_excel(files[1], skip = 2, col_names = new_names,
                                            col_types = ctypes, .name_repair = ~ janitor::make_clean_names))

  #these are the fields to import Note these are different depending on river
  fields <- c("collection_method", "sample_type", "collect_date",
              "project_site_ref", "alkalinity_tot_ca_co3_mg_l",
              "c_sol_org_doc_doc_as_npoc_mg_l", "chlorophyll_a_by_vol_mg_l",
              "n_sum_sol_org_don_mg_l", "n_sum_sol_ox_n_ox_n_ton_mg_l",
              "n_tot_tn_p_tn_mg_l", "nh3_n_nh4_n_sol_mg_l", "o_do_in_situ_mg_l",
              "p_tot_tp_p_tp_mg_l", "po4_p_sol_react_srp_frp_mg_l",
              "salinity_mg_l", "secchi_depth_m", "si_o2_si_sol_react_mg_l",
              "tss_mg_l", "temperature_in_situ_deg_c", "p_h_no_units")

  #function to convert less than greater than symbols to approved numeric values
  conv_l_or_g <- function(x){
    ifelse(str_detect(x, "^<"), as.numeric(sapply(str_split(x, "<"),
                                                  function(x) x[2]))/2,
           ifelse(str_detect(x, "^>"), as.numeric(sapply(str_split(x, ">"),
                                                         function(x) x[2])),
                  as.numeric(x)))
  }

  #dynamic background data date generator
  report_start <- paste0(reportingYear - 6, "-06-01")
  report_fin <- paste0(reportingYear - 1, "-05-31")

  #converting text to numeric and handling less and greater thans
  tidy_all <- suppressWarnings(d1 %>%
                                 select(fields) %>%
                                 mutate(alkalinity_tot_ca_co3_mg_l = conv_l_or_g(alkalinity_tot_ca_co3_mg_l),
                                        c_sol_org_doc_doc_as_npoc_mg_l = conv_l_or_g(c_sol_org_doc_doc_as_npoc_mg_l),
                                        chlorophyll_a_by_vol_mg_l = conv_l_or_g(chlorophyll_a_by_vol_mg_l),
                                        n_sum_sol_org_don_mg_l = conv_l_or_g(n_sum_sol_org_don_mg_l),
                                        n_sum_sol_ox_n_ox_n_ton_mg_l = conv_l_or_g(n_sum_sol_ox_n_ox_n_ton_mg_l),
                                        n_tot_tn_p_tn_mg_l = conv_l_or_g(n_tot_tn_p_tn_mg_l),
                                        nh3_n_nh4_n_sol_mg_l = conv_l_or_g(nh3_n_nh4_n_sol_mg_l),
                                        o_do_in_situ_mg_l = conv_l_or_g(o_do_in_situ_mg_l),
                                        p_tot_tp_p_tp_mg_l = conv_l_or_g(p_tot_tp_p_tp_mg_l),
                                        po4_p_sol_react_srp_frp_mg_l = conv_l_or_g(po4_p_sol_react_srp_frp_mg_l),
                                        salinity_mg_l = conv_l_or_g(salinity_mg_l),
                                        secchi_depth_m = conv_l_or_g(secchi_depth_m),
                                        si_o2_si_sol_react_mg_l = conv_l_or_g(si_o2_si_sol_react_mg_l),
                                        tss_mg_l = conv_l_or_g(tss_mg_l),
                                        temperature_in_situ_deg_c = conv_l_or_g(temperature_in_situ_deg_c),
                                        p_h_no_units = conv_l_or_g(p_h_no_units)))

  #loop to get all downloaded WIN data
  if(length(files) > 1){
    for(i in seq_along(files)[-1]){
      t_names1 <- suppressWarnings(names(readxl::read_excel(files[i])))
      new_names1 <- c(names24, t_names1[25:length(t_names1)])
      ctypes1 <- c(rep("guess", 3), "date", rep("guess", 20), rep("text", length(t_names1) - 24))
      d <- suppressWarnings(readxl::read_excel(files[i], skip = 2, col_names = new_names1,
                                               col_types = ctypes1, .name_repair = ~ janitor::make_clean_names))
      t_d <- suppressWarnings(d %>%
                                select(fields) %>%
                                mutate(alkalinity_tot_ca_co3_mg_l = conv_l_or_g(alkalinity_tot_ca_co3_mg_l),
                                       c_sol_org_doc_doc_as_npoc_mg_l = conv_l_or_g(c_sol_org_doc_doc_as_npoc_mg_l),
                                       chlorophyll_a_by_vol_mg_l = conv_l_or_g(chlorophyll_a_by_vol_mg_l),
                                       n_sum_sol_org_don_mg_l = conv_l_or_g(n_sum_sol_org_don_mg_l),
                                       n_sum_sol_ox_n_ox_n_ton_mg_l = conv_l_or_g(n_sum_sol_ox_n_ox_n_ton_mg_l),
                                       n_tot_tn_p_tn_mg_l = conv_l_or_g(n_tot_tn_p_tn_mg_l),
                                       nh3_n_nh4_n_sol_mg_l = conv_l_or_g(nh3_n_nh4_n_sol_mg_l),
                                       o_do_in_situ_mg_l = conv_l_or_g(o_do_in_situ_mg_l),
                                       p_tot_tp_p_tp_mg_l = conv_l_or_g(p_tot_tp_p_tp_mg_l),
                                       po4_p_sol_react_srp_frp_mg_l = conv_l_or_g(po4_p_sol_react_srp_frp_mg_l),
                                       salinity_mg_l = conv_l_or_g(salinity_mg_l),
                                       secchi_depth_m = conv_l_or_g(secchi_depth_m),
                                       si_o2_si_sol_react_mg_l = conv_l_or_g(si_o2_si_sol_react_mg_l),
                                       tss_mg_l = conv_l_or_g(tss_mg_l),
                                       temperature_in_situ_deg_c = conv_l_or_g(temperature_in_situ_deg_c),
                                       p_h_no_units = conv_l_or_g(p_h_no_units)))
      tidy_all <- dplyr::bind_rows(tidy_all, t_d)
    }
  } else {
    tidy_all <- tidy_all
  }

  #add reporting zones, plot order for months and key for report period
  tidy_all2 <- tidy_all %>%
    filter(collect_date >= report_start) %>%
    mutate(emz = case_when(project_site_ref == "BLA" | project_site_ref == "ARM" | project_site_ref == "HEA" | project_site_ref == "NAR" ~ "lower",
                           project_site_ref == "NIL" | project_site_ref == "STJ" | project_site_ref == "MAY" | project_site_ref == "RON" ~ "middle",
                           project_site_ref == "KIN" | project_site_ref == "SUC" | project_site_ref == "WMP" | project_site_ref == "MSB" ~ "upper",
                           project_site_ref == "JBC" | project_site_ref == "POL" ~ "swan", TRUE ~ "nrz")) %>%
    mutate(mth = lubridate::month(collect_date),
           pord = case_when(mth == 1 ~ 8,
                            mth == 2 ~ 9,
                            mth == 3 ~ 10,
                            mth == 4 ~ 11,
                            mth == 5 ~ 12,
                            mth == 6 ~ 1,
                            mth == 7 ~ 2,
                            mth == 8 ~ 3,
                            mth == 9 ~ 4,
                            mth == 10 ~ 5,
                            mth == 11 ~ 6,
                            mth == 12 ~ 7),
           rep_per = ifelse(collect_date < report_fin, "background", "present"))

  #write out dataset
  write_csv(tidy_all2, path = paste0(outpath, riv, "annual_report_data_for_", reportingYear,".csv"))
}
