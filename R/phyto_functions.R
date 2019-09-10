# Functions to aid in visualising phytoplankton

#' Creates summary phytoplankton data for plotting
#'
#' \code{phyto_groupR} takes a file path to raw phytoplankton xlsx spreadsheets
#'     and creates summary data for use in plotting.
#'
#' @details This is a precursor function to be run prior to using
#'     (\code{\link{phyto_plotR}}). It takes a file path to raw xlsx
#'     phytoplankton data, creates summary counts on specific name groups and
#'     outputs a summary csv file. The groups are, Chlorophytes, Cyanophytes,
#'     Diatoms, Dinoflagellates, Cryptophyta and Other as a catch all.
#'
#'     Summary csv files are exported to a `summaries/` directory and are named
#'     with the date, estuary and the word "summary". If the directory doesnt
#'     exist, it is created.
#'
#' @param pathin a character filepath to the location of the raw phytoplankton
#'     xlsx spreadsheets
#'
#' @param pathout a character filepath to the location of the desired directory
#'     for the summary csv files
#'
#' @examples
#' \dontrun{
#' phyto_groupR(pathin = "C:/path/to/raw_data", pathout = "C:/path/for_export")
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
#' @importFrom tidyr spread
#' @importFrom readr write_csv
#'
#' @export
phyto_groupR <- function(pathin, pathout){
  locations <- phyto_finder(pathin)
  # make folder for output
  folder <- file.path(pathout, "summaries")
  if (!file.exists(folder)) {
    dir.create(folder)
  }
  for(i in seq_along(locations)){
    loc <- locations[i]
    sheet <- readxl::excel_sheets(loc)[stringr::str_sub(stringr::str_to_lower(readxl::excel_sheets(loc)), 1, 1) != "e"]
    dat <- readxl::read_excel(loc, sheet = sheet)
    names(dat) <- tolower(names(dat))
    samp_date <- as.character(dat[[1,1]])
    project <- dat[[1,5]]
    outpath <- paste0(folder, "/")
    short_dat <- dat %>%
      dplyr::select(datecollected, siterefname, species_name, groupname,
                    `species density cells/ml`) %>%
      dplyr::rename(date = datecollected, site = siterefname, species = species_name,
                    group = groupname, density = `species density cells/ml`) %>%
      dplyr::mutate(group = tolower(group)) %>%
      dplyr::mutate(family = case_when(
        str_detect(group, "chloro") | str_detect(group, "prasino") ~ "Chlorophytes",
        str_detect(group, "cryptophyta") ~ "Cryptophyta",
        str_detect(group, "diatoms") ~ "Diatoms",
        str_detect(group, "dinophyta") ~ "Dinoflagellates",
        str_detect(group, "cyanophyta") ~ "Cyanophytes",
        TRUE ~ "Other")) %>%
      dplyr::group_by(date, site, family) %>%
      dplyr::summarise(count = sum(density)) %>%
      tidyr::spread(family, count) %>%
      replace(., is.na(.), 0)
    readr::write_csv(short_dat, paste0(outpath, samp_date, "_", project, "_phyto_summary.csv"))
  }
}

#' Creates barcharts from phytoplankton summary data
#'
#' \code{phyto_plotR} takes a file path to summary data and a date and produces
#'     "present" and "last week" barcharts of phytoplankton counts per site.
#'
#' @details This function can only be run on data created by running the
#'     (\code{\link{phyto_groupR}}) function. It will read in all the summary
#'     data then based on the provided date, produce a barchart for that date
#'     and the prior weeks summary in the one output.
#'
#'     As data is gathered for each estuary on consecutive days, the choice of
#'     date will determine which river summaries will be plotted. That choice
#'     will be reflected in the sites plotted in the barcharts. These barcharts
#'     will be exported (as pdf) to the `summaries/` directory.
#'
#'     Lastly this function will export a "running_datasheet" that contains all
#'     summary data for the estuary prior to and including the date requested.
#'
#' @param summary a character file path to the location of the `summaries/`
#'     directory.
#'
#' @param date a character representation of the date chosen for the "present"
#'     barchart in the format "yyyymmdd".
#'
#' @examples
#' \dontrun{
#' phyto_plotR(summary = "C:/path/to/summary_data", date = "20190910")
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
#' @importFrom ggpubr ggarrange
#' @import dplyr
#' @import ggplot2
#'
#' @export

phyto_plotR <- function(summary, date){
  folder <- file.path(summary, "summaries")
  files <- file.path(folder, list.files(folder, "summary.csv"))
  justfiles <- list.files(folder, "summary.csv")
  dat <- data.frame()
  for(i in seq_along(files)){
    dat1 <- readr::read_csv(files[i])
    dat2 <- tidyr::gather(dat1, "family", "count", 3:max(length(names(dat1))))
    dat2$project <- stringr::str_split(justfiles[i], "_")[[1]][2]
    dat <- dplyr::bind_rows(dat, dat2)
  }

  if(sum(stringr::str_detect(dat$date,
                    as.character(ymd(date)))) > 0 &
     sum(stringr::str_detect(dat$date,
                    as.character(ymd(date) - 7))) > 0){
    current <- lubridate::ymd(date)

    dat$family <- factor(dat$family, levels = c("Other", "Cyanophytes", "Dinoflagellates",
                                                "Chlorophytes", "Cryptophyta", "Diatoms"),
                         ordered = TRUE)

    dat$site<- factor(dat$site, levels = c("BLA", "ARM", "HEA", "NAR", "NIL", "STJ", "MAY",
                                           "RON", "KIN", "SUC", "WMP", "MSB", "SCB2", "SAL",
                                           "RIV", "CASMID", "KEN", "BAC", "NIC", "ELL"),
                      ordered = TRUE)

    now <- dat %>%
      dplyr::filter(date == current)

    prior <- dat %>%
      dplyr::filter(date == current - 7)

    nmax <- now %>%
      dplyr::group_by(site) %>%
      dplyr::summarise(total = sum(count)) %>%
      dplyr::summarise(m = max(total))

    pmax <- prior %>%
      dplyr::group_by(site) %>%
      dplyr::summarise(total = sum(count)) %>%
      dplyr::summarise(m = max(total))

    current_project <- now[1, "project"]

    ymax <- max(nmax, pmax) + 2000

    n_plot <- ggplot(now) +
      geom_bar(aes(x = site, y = count, fill = family), stat = "identity") +
      scale_fill_manual(name = "",
                        values = phyto_cols,
                        limits = c("Diatoms", "Dinoflagellates", "Cyanophytes",
                                   "Chlorophytes", "Cryptophyta", "Other")) +
      ylim(c(0, ymax)) +
      labs(x = "",
           y = "",
           title = format(current, "%d %B %Y")) +
      theme_bw()+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line( size=.1))

    p_plot <- ggplot(prior) +
      geom_bar(aes(x = site, y = count, fill = family), stat = "identity") +
      scale_fill_manual(name = "",
                        values = phyto_cols,
                        limits = c("Diatoms", "Dinoflagellates", "Cyanophytes",
                                   "Chlorophytes", "Cryptophyta", "Other")) +
      ylim(c(0, ymax)) +
      labs(x = "",
           y = "",
           title = format(current - 7, "%d %B %Y")) +
      theme_bw()+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line( size=.1))

    # save the plot
    phytos <- ggpubr::ggarrange(n_plot, p_plot, ncol = 1, common.legend = TRUE, legend = "bottom")
    pdf_name <- file.path(folder, paste0(current,"_", now[1,5],"_summary_plots.pdf"))
    ggsave(plot = phytos, filename = pdf_name)

    # running datasheet
    datout <- dat %>%
      tidyr::spread(family, count) %>%
      dplyr::filter(date <= current & project == current_project) %>%
      replace(., is.na(.), 0)

    outpath <- paste0(folder, "/")
    readr::write_csv(datout, paste0(outpath, current_project, "_running_datasheet.csv"))
  } else {
    stop("There is a problem with the date you have selected")
  }
}
