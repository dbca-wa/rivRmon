# Main function for creating Oxy KPI plots and stats

#' Reads in curated Oxy KPI sonde data and exports cummulative Oxy KPI plots
#' and stats for the purpose of weekly updates.
#'
#' \code{oxy_wranglR} takes a file path to a Project folder that contains
#'     a `data/` directory containing curated dissolved oxygen sonde data.
#'     The function summarises the data producing stats, saved as a csv to
#'     `data/` and KPI plots saved to a `plots/` directory.
#'
#' @details The curated data consists of a csv file that has edited data
#'     appended to it on a weekly basis. The function expects 4 columns:
#'     Site Code, Date, Depth and ODO and the file should have the naming
#'     convention: "River_yyyy-yy_oxy_kpi.csv". Please NOTE that the date format
#'     for the Date column should be "dd-mm-yyyy".
#'
#'     When run the function outputs weekly operational targets and cummulative
#'     annual KPI's for both 2 mg/L and 4 mg/L concentrations of dissolved
#'     oxygen.
#'
#'     The Function also creates 3 plots. A cummulative DO concentration plot,
#'     a DO > 2 mg/L plot and a DO > 4 mg/L plot. Format is PNG in the
#'     knowledge that they will need to be opened in Windows Paint to allow
#'     copying and pasting into a weekly update email.
#'
#' @param path Character string file path to location of the `data/`
#'     directory.
#'
#' @param weir_open Character string representing the date ("yyyy-mm-dd") the
#'     Canning Weir was opened (if appropriate), default: NULL
#'
#' @param weir_closed Character string representing the date ("yyyy-mm-dd") the
#'     Canning Weir was closed (if appropriate), default: NULL
#'
#' @return Three PNG Oxy KPI plots saved to `plots/` directory and a csv
#'     file of the current Oxy KPI stats saved tp `data/` directory.
#'
#' @examples
#' \dontrun{
#' oxy_wranglR(path = "Z:/DEC/MonitoringProgram/Oxy", weir_open = "2018-12-24",
#' weir_closed = NULL)
#' }
#'
#' @import dplyr
#' @importFrom  lubridate dmy ymd
#' @import tidyr
#' @import scales
#'
#' @export

oxy_wranglR <- function(path, weir_open = NULL, weir_closed = NULL){
  ## Read in cleaned csv from data/
  dfile <- list.files(path = file.path(path, "data"), pattern = "oxy_kpi.csv")
  river <- stringr::str_split(dfile, "_")[[1]][1]

  df <- readr::read_csv(file = file.path(path, "data", dfile))

  ## Make summary from data
  summary <- df %>%
    dplyr::mutate(Date = lubridate::dmy(Date)) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarise(Mean = mean(ODO),
                     '10th' = quantile(ODO, probs = 0.1),
                     '90th' = quantile(ODO, probs = 0.9),
                     Min = min(ODO),
                     Max = max(ODO),
                     Samples = n(),
                     W2mgL = sum(ODO > 2),
                     W4mgL = sum(ODO > 4),
                     '2mgL%' = sum(ODO > 2)/Samples * 100,
                     '4mgL%' = sum(ODO > 4)/Samples * 100) %>%
    dplyr::mutate(cumsamp = cumsum(Samples),
                  cum2 = cumsum(W2mgL),
                  cum4 = cumsum(W4mgL),
                  seas2 = cum2/cumsamp * 100,
                  seas4 = cum4/cumsamp * 100) %>%
    dplyr::arrange(Date)

  ## Export KPI
  data_out <- summary %>%
    dplyr::select(Date, '2mgL%', '4mgL%', seas2, seas4) %>%
    dplyr::rename('2mgL perc' = '2mgL%',
                  '4mgL perc' = '4mgL%',
                  '2mgL ann' = seas2,
                  '4mgL ann' = seas4) %>%
    tidyr::gather("desc", "value", 2:5) %>%
    tidyr::separate(desc, into = c("Concentration", "kpi")) %>%
    tidyr::spread(kpi, value) %>%
    dplyr::select(Date, Concentration, perc, ann) %>%
    # dplyr::mutate(perc = round(perc, 2),
    #               ann = round(ann, 2)) %>%
    dplyr::rename('Weekly Op Target' = perc,
                  'Annual KPI' = ann)

  csv_name <- paste0(river, "_current_oxy_kpi_stats.csv")
  write_csv(data_out, path = file.path(path, "data", csv_name))



  ## Insert missing week dates with NA's
  #start with 54 weeks
  all_weeks54 <- seq(summary[[1,1]], by = "week", length.out = 54)
  #remove from tail months of july
  weeks <- data.frame(Date = all_weeks54[1:(54 - sum(month(tail(all_weeks54, 4)) >= 7))])

  full_summary <- dplyr::full_join(weeks, summary, by = "Date") %>%
    dplyr::arrange(Date)



  ## Set up auto complete date range for plot
  #start with 27 fortnights
  dates27 <- seq(full_summary[1,1], by = "2 weeks", length.out = 27)
  #remove from tail months of july
  plot_dates <- dates27[1:(27 - sum(month(tail(dates27, 4)) >= 7))]


  ## Set up horizontal zone colours for weekly means
  weekly_means_rect <- data.frame(state = factor(c("Well Oxygenated",
                                                   "Oxygenated", "Low DO",
                                                   "Hypoxic"),
                                                 levels = c("Well Oxygenated",
                                                            "Oxygenated",
                                                            "Low DO",
                                                            "Hypoxic")),
                                  xmin = full_summary[1,1],
                                  xmax = tail(full_summary[,1], 1),
                                  ymin = c(6, 4, 2, 0),
                                  ymax = c(12, 6, 4, 2),
                                  stringsAsFactors = FALSE)

  ## Set up horizontal zone colours for weekly > 2mg/L
  weekly_2_rect <- data.frame(state = factor(c("Good", "Acceptable","Review"),
                                             levels = c("Good", "Acceptable",
                                                        "Review")),
                              xmin = full_summary[1,1],
                              xmax = tail(full_summary[,1], 1),
                              ymin = c(90, 80, 40),
                              ymax = c(100, 90, 80),
                              stringsAsFactors = FALSE)

  ## Set up horizontal zone colours for weekly > 4mg/L
  weekly_4_rect <- data.frame(state = factor(c("Good", "Acceptable","Review"),
                                             levels = c("Good", "Acceptable",
                                                        "Review")),
                              xmin = full_summary[1,1],
                              xmax = tail(full_summary[,1], 1),
                              ymin = c(80, 70, 40),
                              ymax = c(100, 80, 70),
                              stringsAsFactors = FALSE)

  ## Plots
  ## Weekly DO conc mgL
  w1 <- ggplot() +
    geom_point(data = full_summary, aes(x = Date, y = Mean, colour = "blue")) +
    geom_line(data = full_summary, aes(x = Date, y = `10th`,
                                       colour = "red"), linetype = 2, size = 0.8) +
    geom_line(data = full_summary, aes(x = Date, y = `90th`,
                                       colour = "darkgreen"), linetype = 2, size = 0.8) +
    geom_rect(data = weekly_means_rect, aes(xmin = xmin, xmax = xmax,
                                            ymin = ymin, ymax = ymax,
                                            fill = state), alpha = 0.4) +
    geom_vline(xintercept = c(ymd(weir_open), ymd(weir_closed)), colour = "red", linetype = 2) +
    scale_fill_manual(name = "",
                      values = c("#72DD6F", "#6C9DF8", "#EDEF74", "#F4B761")) +
    scale_colour_manual(name = "",
                        labels = c("Mean DO", "90%", "10%"),
                        values = c( "blue", "darkgreen", "red")) +
    scale_y_continuous(breaks = seq(0, 12, by = 2),
                       labels = seq(0, 12, by = 2),
                       limits = c(0, 12),
                       expand = c(0, 0)) +
    scale_x_date(breaks = plot_dates,
                 #limits = c(ymd("2018-07-02"), ymd("2019-06-24")),
                 expand = c(0.01, 0)) +
    labs(x = "",
         y = "",
         title = "Weekly Dissolved Oxygen Concentration (mg/L)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.4),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.background = element_blank(),
          panel.border=element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.direction = "horizontal",
          legend.position = "bottom",
          legend.key.size =  unit(6, "mm"),
          legend.title = element_blank(),
          legend.text = element_text(size = 6)) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE),
           colour = guide_legend(override.aes = list(linetype = c(0, 2, 2),
                                                     shape = c(16, NA, NA))))


  ## Weekly % dissolved Oxygen >2 mg/L
  w2 <- ggplot() +
    geom_point(data = full_summary, aes(x = Date, y = `2mgL%`, colour = "yellow")) +
    geom_line(data = full_summary, aes(x = Date, y = seas2,
                                       colour = "black"), size = 0.8) +
    geom_rect(data = weekly_2_rect, aes(xmin = xmin, xmax = xmax,
                                        ymin = ymin, ymax = ymax,
                                        fill = state), alpha = 0.4) +

    geom_vline(xintercept = c(ymd(weir_open), ymd(weir_closed)), colour = "red", linetype = 2) +
    scale_fill_manual(name = "",
                      values = c("#72DD6F", "#EDEF74", "#F4B761")) +
    scale_colour_manual(name = "",
                        labels = c("Weekly %", "Cummulative annual %"),
                        values = c( "black", "black")) +
    scale_y_continuous(breaks = seq(40, 100, by = 10),
                       labels = seq(40, 100, by = 10),
                       limits = c(40, 101),
                       expand = c(0, 0.4)) +
    scale_x_date(breaks = plot_dates,
                 #limits = c(ymd("2018-07-02"), ymd("2019-06-24")),
                 expand = c(0.01, 0)) +
    labs(x = "",
         y = "",
         title = "% Dissolved Oxygen Concentration > 2mg/L") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.4),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.background = element_blank(),
          panel.border=element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.direction = "horizontal",
          legend.position = "bottom",
          legend.key.size =  unit(6, "mm"),
          legend.title = element_blank(),
          legend.text = element_text(size = 6)) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE),
           colour = guide_legend(override.aes = list(linetype = c(0, 1),
                                                     shape = c(16, NA))))

  ## Weekly % dissolved Oxygen >4 mg/L
  w3 <- ggplot() +
    geom_point(data = full_summary, aes(x = Date, y = `4mgL%`, colour = "yellow")) +
    geom_line(data = full_summary, aes(x = Date, y = seas4,
                                       colour = "black"), size = 0.8) +
    geom_rect(data = weekly_4_rect, aes(xmin = xmin, xmax = xmax,
                                        ymin = ymin, ymax = ymax,
                                        fill = state), alpha = 0.4) +

    geom_vline(xintercept = c(ymd(weir_open), ymd(weir_closed)), colour = "red", linetype = 2) +
    scale_fill_manual(name = "",
                      values = c("#72DD6F", "#EDEF74", "#F4B761")) +
    scale_colour_manual(name = "",
                        labels = c("Weekly %", "Cummulative annual %"),
                        values = c( "black", "black")) +
    scale_y_continuous(breaks = seq(40, 100, by = 10),
                       labels = seq(40, 100, by = 10),
                       limits = c(40, 101),
                       expand = c(0, 0.4)) +
    scale_x_date(breaks = plot_dates,
                 #limits = c(ymd("2018-07-02"), ymd("2019-06-24")),
                 expand = c(0.01, 0)) +
    labs(x = "",
         y = "",
         title = "% Dissolved Oxygen Concentration > 4mg/L") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.4),
          #panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #panel.background = element_blank(),
          panel.border=element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.direction = "horizontal",
          legend.position = "bottom",
          legend.key.size =  unit(6, "mm"),
          legend.title = element_blank(),
          legend.text = element_text(size = 6)) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE),
           colour = guide_legend(override.aes = list(linetype = c(0, 1),
                                                     shape = c(16, NA))))

  ## set up plot folder
  folder <- file.path(path, "plots")
  if (!file.exists(folder)) {
    dir.create(folder)
  }
  current_date <- tail(summary[["Date"]], 1)

  w1_name <- paste0(folder, "/", river, "_DO_mgL_", current_date, ".png")
  w2_name <- paste0(folder, "/", river, "_DO_2mgL_perc_", current_date, ".png")
  w3_name <- paste0(folder, "/", river, "_DO_4mgL_perc_", current_date, ".png")

  ggsave(w1, filename = w1_name, width = 6, height = 3.8, units = "in")
  ggsave(w2, filename = w2_name, width = 6, height = 3.8, units = "in")
  ggsave(w3, filename = w3_name, width = 6, height = 3.8, units = "in")

}
