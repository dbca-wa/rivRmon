# Main functions for creating Canning River annual report plots and tables.

#' Function for annual Canning River report plots and tables for group 1
#' metrics.
#'
#' \code{ann_rep_grp1_c} takes an export filepath, data, surface and bottom
#'     plot colours and produces panel plots and tables for group 1 metrics.
#'
#' @details Group 1 metrics are a grouping based on a common collection
#'     method, sample type and display parameters and in this case includes,
#'     total nitrogen, ammonia nitrogen, total oxidised nitrogen, dissolved
#'     organic nitrogen, total phosphorous and filterable reactive phosphorous.
#'     Outputs will be exported to two folders at the outpath location.
#'     `c_panels/` for plots and `c_tables/` for data tables. These will need to
#'     be created if they don't exist.
#'
#' @param outpath filepath to desired export location.
#'
#' @param data the data object created from running `canning_WIN_report_data`.
#'
#' @param surface colour for surface plots. Can be named colour or hex format.
#'
#' @param bottom colour for bottom plots. Can be named colour or hex format.
#'
#' @return a separate panel plot for each metric and a csv of metrics for
#'     inclusion to a table.
#'
#'  @example
#' \dontrun{
#' ann_rep_grp1_c(outpath, data, surface = "blue", bottom = "red")}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @import tidyverse
#' @import lubridate
#' @import ggthemes
#' @import grid
#' @import gridExtra
#'
#' @export

ann_rep_grp1_c <- function(outpath, data, surface, bottom){

  # create output folders paths
  panels <- file.path(outpath, "c_panels")
  tables <- file.path(outpath, "c_tables")

  ##grp1 different to swan (no silica)
  grp1 <- c("n_tot_tn_p_tn_mg_l", "nh3_n_nh4_n_sol_mg_l",
            "n_sum_sol_ox_n_ox_n_ton_mg_l", "n_sum_sol_org_don_mg_l",
            "p_tot_tp_p_tp_mg_l", "po4_p_sol_react_srp_frp_mg_l")

  # data for grp1 for plotting
  dfgrp1 <- data %>%
    dplyr::filter(emz != "nrz" & collection_method == "Grab sample" &
                    sample_type != "Standard") %>%
    tidyr::gather(xvar, value, grp1) %>%
    dplyr::group_by(emz, pord, sample_type, rep_per, xvar) %>%
    dplyr::summarise(lower = quantile(value, probs = 0.10, na.rm = TRUE),
                     upper = quantile(value, probs = 0.90, na.rm = TRUE),
                     median = median(value, na.rm = TRUE)) %>%
    dplyr::mutate(filt = paste(stringr::str_sub(emz, 1, 1),
                               stringr::str_to_lower(stringr::str_sub(sample_type, 1, 1)),
                               stringr::str_sub(rep_per, 1, 1),
                               sep = "."))

  # data for grp1 for table summaries only present period
  dfgrp1tbl <- data %>%
    dplyr::filter(emz != "nrz" & collection_method == "Grab sample" &
                    sample_type != "Standard", rep_per == "present") %>%
    tidyr::gather(xvar, value, grp1) %>%
    dplyr::select(emz, pord, sample_type, xvar, value, mth)

  # mathy labels for plotting
  labgrp1names <- list(c(bquote("TN (mg  "*L^-1*")"), "TN"),
                       c(bquote(NH[3]*"-N (mg  "*L^-1*")"), bquote(NH[3]-N)),
                       c(bquote("NOx-N (mg  "*L^-1*")"), "NOx-N"),
                       c(bquote("DOrGN (mg  "*L^-1*")"), "DOrGN"),
                       c(bquote("TP (mg  "*L^-1*")"), "TP"),
                       c(bquote("FRP (mg  "*L^-1*")"), "FRP"))

  savenames <- c("TN", "NH3-N", "NOX-N", "DOrGN", "TP", "FRP")

  csv_out <- data.frame(stringsAsFactors = FALSE)

  for(i in seq_along(grp1)){

    dfgrp1.i <- dplyr::filter(dfgrp1, xvar == grp1[i])

    dfgrp1tbl.i <- dplyr::filter(dfgrp1tbl, xvar == grp1[i]) %>%
      dplyr::mutate(
        sample = factor(case_when(
          sample_type == "Bottom sample" ~ "Bottom",
          sample_type == "Surface sample" ~ "Surface"
        ), levels = c("Surface", "Bottom"))) %>%
      dplyr::group_by(emz, sample, pord) %>%
      dplyr::summarise(n = n(),
                       min = min(value),
                       max = max(value),
                       median = median(value)) %>%
      dplyr::mutate(
        mth = factor(case_when(
          pord == 1 ~ "Jun",
          pord == 2 ~ "Jul",
          pord == 3 ~ "Aug",
          pord == 4 ~ "Sep",
          pord == 5 ~ "Oct",
          pord == 6 ~ "Nov",
          pord == 7 ~ "Dec",
          pord == 8 ~ "Jan",
          pord == 9 ~ "Feb",
          pord == 10 ~ "Mar",
          pord == 11 ~ "Apr",
          pord == 12 ~ "May"
        ), levels = c("Jun", "Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb",
                      "Mar", "Apr","May"))) %>%
      dplyr::select(-pord) %>%
      tidyr::gather("metric", "value", 3:6) %>%
      dplyr::mutate(metric = factor(metric, levels = c("n", "min", "max", "median")),
                    variable = grp1[i]) %>%
      tidyr::spread(mth, value)

    csv_out <- dplyr::bind_rows(csv_out, dfgrp1tbl.i)

    # y limits finder
    dyn_ylim <- dynamic_ylim(dfgrp1.i)

    a <- ggplot(data = dplyr::filter(dfgrp1.i, filt == "e.s.b"), aes(pord)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = surface, alpha = 0.1) +
      geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
      geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
      geom_line(aes(x = pord, y = median), colour = surface) +
      geom_point(data = dplyr::filter(dfgrp1.i, filt == "e.s.p"), aes(x = pord, y = median), pch = 1, size = 1) +
      labs(y = labgrp1names[[i]][[1]]) +
      scale_y_continuous(limits = dyn_ylim) +
      scale_x_continuous(name = bquote((a) ~.(labgrp1names[[i]][[2]]) ~ Canning ~ Estuary ~(Surface)),
                         breaks = c(8:12, 1:7),
                         labels = month.abb[1:12]) +
      theme_bw() +
      theme(panel.grid.major = element_line(linetype = "blank"),
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
            axis.text.x = element_text(size = 6),
            axis.title.y = element_text(size = 9),
            axis.text.y = element_text(size = 7))

    c <- ggplot(data = dplyr::filter(dfgrp1.i, filt == "r.s.b"), aes(pord)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = surface, alpha = 0.1) +
      geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
      geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
      geom_line(aes(x = pord, y = median), colour = surface) +
      geom_point(data = dplyr::filter(dfgrp1.i, filt == "r.s.p"), aes(x = pord, y = median), pch = 1, size = 1) +
      labs(y = labgrp1names[[i]][[1]]) +
      scale_y_continuous(limits = dyn_ylim) +
      scale_x_continuous(name = bquote((c) ~ .(labgrp1names[[i]][[2]]) ~ Lower ~ Canning ~ River ~(Surface)),
                         breaks = c(8:12, 1:7),
                         labels = month.abb[1:12]) +
      theme_bw() +
      theme(panel.grid.major = element_line(linetype = "blank"),
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
            axis.text.x = element_text(size = 6),
            axis.title.y = element_text(size = 9),
            axis.text.y = element_text(size = 7))

    c_leg <- ggplot(data = dplyr::filter(dfgrp1.i, filt == "r.s.b"), aes(pord)) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = surface), alpha = 0.1) +
      scale_fill_manual(name = "",
                        values = surface,
                        labels = "Monthly background 10th and 90th percentile") +
      geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = surface, alpha = 0.4) +
      geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = surface, alpha = 0.4) +
      geom_line(aes(x = pord, y = median, colour = surface)) +
      scale_colour_manual(name = "",
                          values = surface,
                          labels = "Monthly background Median") +
      geom_point(data = dplyr::filter(dfgrp1.i, filt == "r.s.p"), aes(x = pord, y = median, shape = filt),
                 size = 2) +
      scale_shape_manual(name = "",
                         values = 1,
                         labels = "18/19 monthly Median") +
      labs(y = labgrp1names[[i]][[1]]) +
      scale_y_continuous(limits = dyn_ylim) +
      scale_x_continuous(name = "",
                         breaks = c(8:12, 1:7),
                         labels = month.abb[1:12]) +
      theme_bw() +
      theme(legend.position = "bottom", legend.box = "vertical",
            legend.box.just = "left",
            legend.spacing.y = unit(2, 'mm'),
            legend.spacing.x = unit(2, 'mm'),
            legend.key.size = unit(3, "mm"),
            panel.grid.major = element_line(linetype = "blank"),
            panel.grid.minor = element_line(linetype = "blank")) +
      guides(fill = guide_legend(order = 1),
             colour = guide_legend(order = 2),
             shape = guide_legend(order = 3))

    legend_surf <- get_legend(c_leg)

    b <- ggplot(data = dplyr::filter(dfgrp1.i, filt == "e.b.b"), aes(pord)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = bottom, alpha = 0.1) +
      geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = bottom, alpha = 0.4) +
      geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = bottom, alpha = 0.4) +
      geom_line(aes(x = pord, y = median), colour = bottom) +
      geom_point(data = dplyr::filter(dfgrp1.i, filt == "e.b.p"), aes(x = pord, y = median), size = 1) +
      labs(y = labgrp1names[[i]][[1]]) +
      scale_y_continuous(limits = dyn_ylim) +
      scale_x_continuous(name = bquote((b) ~ .(labgrp1names[[i]][[2]]) ~ Canning ~ Estuary ~(Bottom)),
                         breaks = c(8:12, 1:7),
                         labels = month.abb[1:12]) +
      theme_bw() +
      theme(panel.grid.major = element_line(linetype = "blank"),
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
            axis.text.x = element_text(size = 6),
            axis.title.y = element_text(size = 9),
            axis.text.y = element_text(size = 7))

    d <- ggplot(data = dplyr::filter(dfgrp1.i, filt == "r.b.b"), aes(pord)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = bottom, alpha = 0.1) +
      geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = bottom, alpha = 0.4) +
      geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = bottom, alpha = 0.4) +
      geom_line(aes(x = pord, y = median), colour = bottom) +
      geom_point(data = dplyr::filter(dfgrp1.i, filt == "r.b.p"), aes(x = pord, y = median), size = 1) +
      labs(y = labgrp1names[[i]][[1]]) +
      scale_y_continuous(limits = dyn_ylim) +
      scale_x_continuous(name = bquote((d) ~ .(labgrp1names[[i]][[2]]) ~ Lower ~ Canning ~ River ~(Bottom)),
                         breaks = c(8:12, 1:7),
                         labels = month.abb[1:12]) +
      theme_bw() +
      theme(panel.grid.major = element_line(linetype = "blank"),
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
            axis.text.x = element_text(size = 6),
            axis.title.y = element_text(size = 9),
            axis.text.y = element_text(size = 7))

    d_leg <- ggplot(data = dplyr::filter(dfgrp1.i, filt == "r.b.b"), aes(pord)) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = bottom), alpha = 0.1) +
      scale_fill_manual(name = "",
                        values = bottom,
                        labels = "Monthly background 10th-90th percentile") +
      geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = bottom, alpha = 0.4) +
      geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = bottom, alpha = 0.4) +
      geom_line(aes(x = pord, y = median, colour = bottom)) +
      scale_colour_manual(name = "",
                          values = bottom,
                          labels = "Monthly background Median") +
      geom_point(data = dplyr::filter(dfgrp1.i, filt == "r.b.p"), aes(x = pord, y = median, shape = filt),
                 size = 2) +
      scale_shape_manual(name = "",
                         values = 16,
                         labels = "18/19 monthly Median") +
      labs(y = labgrp1names[[i]][[1]]) +
      scale_y_continuous(limits = dyn_ylim) +
      scale_x_continuous(name = "",
                         breaks = c(8:12, 1:7),
                         labels = month.abb[1:12]) +
      theme_bw() +
      theme(legend.position = "bottom", legend.box = "vertical",
            legend.box.just = "left",
            legend.spacing.y = unit(2, 'mm'),
            legend.spacing.x = unit(2, 'mm'),
            legend.key.size = unit(3, "mm"),
            panel.grid.major = element_line(linetype = "blank"),
            panel.grid.minor = element_line(linetype = "blank")) +
      guides(fill = guide_legend(order = 1),
             colour = guide_legend(order = 2),
             shape = guide_legend(order = 3))

    legend_bott <- get_legend(d_leg)

    out <- gridExtra::grid.arrange(a, b, c, d, legend_surf, legend_bott, ncol = 2)

    outname <- paste0(panels, "/", savenames[i], "_panel_grp1_canning.pdf")

    gout <- gtable::gtable_add_padding(out, padding = unit(c(1,2,10,2), "cm"))
    ggsave(filename = outname, plot = gout, width = 10, height = 14,
           units = "in", useDingbats=FALSE)
  }
  csvname <- paste0(tables, "/output_tables_grp1_canning.csv")
  write_csv(csv_out, path = csvname)
}

#' Function for annual Canning River report plots and tables for silica.
#'
#' \code{ann_rep_si_c} takes an export filepath, data, and surface
#'     plot colour and produces panel plots and tables for silica.
#'
#' @details Outputs will be exported to two folders at the outpath
#'     location. `c_panels/` for plots and `c_tables/` for data tables. These
#'     will need to be created if they don't exist.
#'
#' @param outpath filepath to desired export location.
#'
#' @param data the data object created from running `canning_WIN_report_data`.
#'
#' @param surface colour for surface plots. Can be named colour or hex format.
#'
#' @return a  panel plot for silica and a csv of metrics for
#'     inclusion to a table.
#'
#'  @example
#' \dontrun{
#' ann_rep_si_c(outpath, data, surface = "blue")}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @import tidyverse
#' @import lubridate
#' @import ggthemes
#' @import grid
#' @import gridExtra
#'
#' @export

ann_rep_si_c <- function(outpath, data, surface){
  # create output folders paths
  panels <- file.path(outpath, "c_panels")
  tables <- file.path(outpath, "c_tables")

  silica <- "si_o2_si_sol_react_mg_l"

  # data for silica for plotting
  dfsilica <- data %>%
    dplyr::filter(emz != "nrz" & collection_method == "Grab sample" &
                    sample_type == "Surface sample") %>%
    tidyr::gather(xvar, value, silica) %>%
    dplyr::group_by(emz, pord, sample_type, rep_per, xvar) %>%
    dplyr::summarise(lower = quantile(value, probs = 0.10, na.rm = TRUE),
                     upper = quantile(value, probs = 0.90, na.rm = TRUE),
                     median = median(value, na.rm = TRUE)) %>%
    dplyr::mutate(filt = paste(stringr::str_sub(emz, 1, 1),
                               stringr::str_to_lower(stringr::str_sub(sample_type, 1, 1)),
                               stringr::str_sub(rep_per, 1, 1),
                               sep = "."))

  # mathy labels for plotting
  labsilicanames <- list(c(bquote(Si0[2]*"-Si (mg  "*L^-1*")"), bquote(Si0[2]-Si)))

  savenames <- "SiO2-Si"

  # data for same silica for table summaries only present period
  dfsilicatbl <- data %>%
    dplyr::filter(emz != "nrz" & collection_method == "Grab sample" &
                    sample_type == "Surface sample", rep_per == "present") %>%
    tidyr::gather(xvar, value, silica) %>%
    dplyr::select(emz, pord, sample_type, xvar, value, mth) %>%
    dplyr::mutate(
      sample = factor(case_when(
        sample_type == "Surface sample" ~ "Surface"),
        levels = c("Surface"))) %>%
    dplyr::group_by(emz, sample, pord) %>%
    dplyr::summarise(n = n(),
                     min = min(value, na.rm = TRUE),
                     max = max(value, na.rm = TRUE),
                     median = median(value, na.rm = TRUE)) %>%
    dplyr::mutate(
      mth = factor(case_when(
        pord == 1 ~ "Jun",
        pord == 2 ~ "Jul",
        pord == 3 ~ "Aug",
        pord == 4 ~ "Sep",
        pord == 5 ~ "Oct",
        pord == 6 ~ "Nov",
        pord == 7 ~ "Dec",
        pord == 8 ~ "Jan",
        pord == 9 ~ "Feb",
        pord == 10 ~ "Mar",
        pord == 11 ~ "Apr",
        pord == 12 ~ "May"
      ), levels = c("Jun", "Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb",
                    "Mar", "Apr","May"))) %>%
    dplyr::select(-pord) %>%
    tidyr::gather("metric", "value", 3:6) %>%
    dplyr::mutate(metric = factor(metric, levels = c("n", "min", "max", "median")),
                  variable = "si_o2_si_sol_react_mg_l") %>%
    dplyr::mutate(value = case_when(
      value < 0.5 ~ "<0.5",
      TRUE ~ as.character(value)
    )) %>%
    tidyr::spread(mth, value)

  csv_out <- dfsilicatbl

  # y limits finder
  dyn_ylim <- dynamic_ylim(dfsilica)

  a <- ggplot(data = dplyr::filter(dfsilica, filt == "e.s.b" & sample_type == "Surface sample"),
              aes(pord)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = surface, alpha = 0.1) +
    geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = median), colour = surface) +
    geom_point(data = dplyr::filter(dfsilica, filt == "e.s.p"& sample_type == "Surface sample"),
               aes(x = pord, y = median), pch = 1, size = 1) +
    labs(y = labsilicanames[[1]][[1]]) +
    scale_y_continuous(limits = dyn_ylim) +
    scale_x_continuous(name = bquote((a) ~.(labsilicanames[[1]][[2]]) ~ Canning ~ Estuary ~(Surface)),
                       breaks = c(8:12, 1:7),
                       labels = month.abb[1:12]) +
    theme_bw() +
    theme(panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank"),
          axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
          axis.text.x = element_text(size = 6),
          axis.title.y = element_text(size = 9),
          axis.text.y = element_text(size = 7))

  b <- ggplot(data = dplyr::filter(dfsilica, filt == "r.s.b" & sample_type == "Surface sample"),
              aes(pord)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = surface, alpha = 0.1) +
    geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = median), colour = surface) +
    geom_point(data = dplyr::filter(dfsilica, filt == "r.s.p" & sample_type == "Surface sample"),
               aes(x = pord, y = median), pch = 1, size = 1) +
    labs(y = labsilicanames[[1]][[1]]) +
    scale_y_continuous(limits = dyn_ylim) +
    scale_x_continuous(name = bquote((b) ~ .(labsilicanames[[1]][[2]]) ~ Lower ~ Canning ~ River ~(Surface)),
                       breaks = c(8:12, 1:7),
                       labels = month.abb[1:12]) +
    theme_bw() +
    theme(panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank"),
          axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
          axis.text.x = element_text(size = 6),
          axis.title.y = element_text(size = 9),
          axis.text.y = element_text(size = 7))

  b_leg <- ggplot(data = dplyr::filter(dfsilica, filt == "r.s.b" & sample_type == "Surface sample"),
                  aes(pord)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = surface), alpha = 0.1) +
    scale_fill_manual(name = "",
                      values = surface,
                      labels = "Monthly background 10th and 90th percentile") +
    geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = median, colour = surface)) +
    scale_colour_manual(name = "",
                        values = surface,
                        labels = "Monthly background Median") +
    geom_point(data = dplyr::filter(dfsilica, filt == "r.s.p" & sample_type == "Surface sample"),
               aes(x = pord, y = median, shape = filt),
               size = 2) +
    scale_shape_manual(name = "",
                       values = 1,
                       labels = "18/19 monthly Median") +
    labs(y = labsilicanames[[1]][[1]]) +
    scale_y_continuous(limits = dyn_ylim) +
    scale_x_continuous(name = "",
                       breaks = c(8:12, 1:7),
                       labels = month.abb[1:12]) +
    theme_bw() +
    theme(legend.position = "bottom", legend.box = "vertical",
          legend.box.just = "left",
          legend.spacing.y = unit(2, 'mm'),
          legend.spacing.x = unit(2, 'mm'),
          legend.key.size = unit(3, "mm"),
          panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank")) +
    guides(fill = guide_legend(order = 1),
           colour = guide_legend(order = 2),
           shape = guide_legend(order = 3))

  legend_surf <- get_legend(b_leg)

  out <- gridExtra::grid.arrange(a, b, legend_surf, ncol = 1)
  outname <- paste0(panels, "/", savenames[1], "_panel_canning.pdf")

  #dims for single column
  gout <- gtable::gtable_add_padding(out, padding = unit(c(1,7.5,10,7.5), "cm"))
  ggsave(filename = outname, plot = gout, width = 10, height = 14,
         units = "in", useDingbats=FALSE)

  csvname <- paste0(tables, "/output_tables_silica_canning.csv")
  write_csv(csv_out, path = csvname)
}

#' Function for annual Canning River report plots and tables for dissolved
#' organic carbon.
#'
#' \code{ann_rep_doc_c} takes an export filepath, data, and surface
#'     plot colour and produces panel plots and tables for dissolved organic
#'     carbon
#'
#' @details Outputs will be exported to two folders  at the outpath
#'     location. `c_panels/` for plots and `c_tables/` for data tables. These
#'     will need to be created if they don't exist.
#'
#' @param outpath filepath to desired export location.
#'
#' @param data the data object created from running `canning_WIN_report_data`.
#'
#' @param surface colour for surface plots. Can be named colour or hex format.
#'
#' @return a  panel plot for dissolved organic carbon and a csv of metrics for
#'     inclusion to a table.
#'
#'  @example
#' \dontrun{
#' ann_rep_doc_c(outpath, data, surface = "blue")}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @import tidyverse
#' @import lubridate
#' @import ggthemes
#' @import grid
#' @import gridExtra
#'
#' @export

ann_rep_doc_c <- function(outpath, data, surface){
  # create output folders paths
  panels <- file.path(outpath, "c_panels")
  tables <- file.path(outpath, "c_tables")

  doc <- "c_sol_org_doc_doc_as_npoc_mg_l"

  # data for doc for plotting
  dfdoc <- data %>%
    dplyr::filter(emz != "nrz" & collection_method == "Grab sample" &
                    sample_type == "Surface sample") %>%
    tidyr::gather(xvar, value, doc) %>%
    dplyr::group_by(emz, pord, sample_type, rep_per, xvar) %>%
    dplyr::summarise(lower = quantile(value, probs = 0.10, na.rm = TRUE),
                     upper = quantile(value, probs = 0.90, na.rm = TRUE),
                     median = median(value, na.rm = TRUE)) %>%
    dplyr::mutate(filt = paste(stringr::str_sub(emz, 1, 1),
                               stringr::str_to_lower(stringr::str_sub(sample_type, 1, 1)),
                               stringr::str_sub(rep_per, 1, 1),
                               sep = "."))

  # mathy labels for plotting
  labdocnames <- list(c(bquote("DOC (mg  "*L^-1*")"), "DOC"))

  savenames <- "DOC"

  # data for same doc for table summaries only present period
  dfdoctbl <- data %>%
    dplyr::filter(emz != "nrz" & collection_method == "Grab sample" &
                    sample_type == "Surface sample", rep_per == "present") %>%
    tidyr::gather(xvar, value, doc) %>%
    dplyr::select(emz, pord, sample_type, xvar, value, mth) %>%
    dplyr::mutate(
      sample = factor(case_when(
        sample_type == "Surface sample" ~ "Surface"),
        levels = c("Surface"))) %>%
    dplyr::group_by(emz, sample, pord) %>%
    dplyr::summarise(n = n(),
                     min = min(value, na.rm = TRUE),
                     max = max(value, na.rm = TRUE),
                     median = median(value, na.rm = TRUE)) %>%
    dplyr::mutate(
      mth = factor(case_when(
        pord == 1 ~ "Jun",
        pord == 2 ~ "Jul",
        pord == 3 ~ "Aug",
        pord == 4 ~ "Sep",
        pord == 5 ~ "Oct",
        pord == 6 ~ "Nov",
        pord == 7 ~ "Dec",
        pord == 8 ~ "Jan",
        pord == 9 ~ "Feb",
        pord == 10 ~ "Mar",
        pord == 11 ~ "Apr",
        pord == 12 ~ "May"
      ), levels = c("Jun", "Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb",
                    "Mar", "Apr","May"))) %>%
    dplyr::select(-pord) %>%
    tidyr::gather("metric", "value", 3:6) %>%
    dplyr::mutate(metric = factor(metric, levels = c("n", "min", "max", "median")),
                  variable = "c_sol_org_doc_doc_as_npoc_mg_l") %>%
    dplyr::mutate(value = case_when(
      value < 0.5 ~ "<0.5",
      TRUE ~ as.character(value)
    )) %>%
    tidyr::spread(mth, value)

  csv_out <- dfdoctbl

  # y limits finder
  dyn_ylim <- dynamic_ylim(dfdoc)

  a <- ggplot(data = dplyr::filter(dfdoc, filt == "e.s.b" & sample_type == "Surface sample"),
              aes(pord)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = surface, alpha = 0.1) +
    geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = median), colour = surface) +
    geom_point(data = dplyr::filter(dfdoc, filt == "e.s.p"& sample_type == "Surface sample"),
               aes(x = pord, y = median), pch = 1, size = 1) +
    labs(y = labdocnames[[1]][[1]]) +
    scale_y_continuous(limits = dyn_ylim) +
    scale_x_continuous(name = bquote((a) ~.(labdocnames[[1]][[2]]) ~ Canning ~ Estuary ~(Surface)),
                       breaks = c(8:12, 1:7),
                       labels = month.abb[1:12]) +
    theme_bw() +
    theme(panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank"),
          axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
          axis.text.x = element_text(size = 6),
          axis.title.y = element_text(size = 9),
          axis.text.y = element_text(size = 7))

  b <- ggplot(data = dplyr::filter(dfdoc, filt == "r.s.b" & sample_type == "Surface sample"),
              aes(pord)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = surface, alpha = 0.1) +
    geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = median), colour = surface) +
    geom_point(data = dplyr::filter(dfdoc, filt == "r.s.p" & sample_type == "Surface sample"),
               aes(x = pord, y = median), pch = 1, size = 1) +
    labs(y = labdocnames[[1]][[1]]) +
    scale_y_continuous(limits = dyn_ylim) +
    scale_x_continuous(name = bquote((b) ~ .(labdocnames[[1]][[2]]) ~ Lower ~ Canning ~ River ~(Surface)),
                       breaks = c(8:12, 1:7),
                       labels = month.abb[1:12]) +
    theme_bw() +
    theme(panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank"),
          axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
          axis.text.x = element_text(size = 6),
          axis.title.y = element_text(size = 9),
          axis.text.y = element_text(size = 7))

  b_leg <- ggplot(data = dplyr::filter(dfdoc, filt == "r.s.b" & sample_type == "Surface sample"),
                  aes(pord)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = surface), alpha = 0.1) +
    scale_fill_manual(name = "",
                      values = surface,
                      labels = "Monthly background 10th and 90th percentile") +
    geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = median, colour = surface)) +
    scale_colour_manual(name = "",
                        values = surface,
                        labels = "Monthly background Median") +
    geom_point(data = dplyr::filter(dfdoc, filt == "r.s.p" & sample_type == "Surface sample"),
               aes(x = pord, y = median, shape = filt),
               size = 2) +
    scale_shape_manual(name = "",
                       values = 1,
                       labels = "18/19 monthly Median") +
    labs(y = labdocnames[[1]][[1]]) +
    scale_y_continuous(limits = dyn_ylim) +
    scale_x_continuous(name = "",
                       breaks = c(8:12, 1:7),
                       labels = month.abb[1:12]) +
    theme_bw() +
    theme(legend.position = "bottom", legend.box = "vertical",
          legend.box.just = "left",
          legend.spacing.y = unit(2, 'mm'),
          legend.spacing.x = unit(2, 'mm'),
          legend.key.size = unit(3, "mm"),
          panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank")) +
    guides(fill = guide_legend(order = 1),
           colour = guide_legend(order = 2),
           shape = guide_legend(order = 3))

  legend_surf <- get_legend(b_leg)

  out <- gridExtra::grid.arrange(a, b, legend_surf, ncol = 1)
  outname <- paste0(panels, "/", savenames[1], "_panel_canning.pdf")

  #dims for single column
  gout <- gtable::gtable_add_padding(out, padding = unit(c(1,7.5,10,7.5), "cm"))
  ggsave(filename = outname, plot = gout, width = 10, height = 14,
         units = "in", useDingbats=FALSE)

  csvname <- paste0(tables, "/output_tables_doc_canning.csv")
  write_csv(csv_out, path = csvname)
}

#' Function for annual Canning River report plots and tables for group 3
#' metrics
#'
#' \code{ann_rep_grp3_c} takes an export filepath, data, surface and bottom
#'     plot colours and produces panel plots and tables for group 3 metrics.
#'
#' @details Group 3 metrics are a grouping based on a common collection
#'     method, sample type and display parameters and in this case includes,
#'     dissolved oxygen, salinity, temperature, and pH.
#'     Outputs will be exported to two folders at the outpath location.
#'     `c_panels/` for plots and `c_tables/` for data tables. These will need to
#'     be created if they don't exist.
#'
#' @param outpath filepath to desired export location.
#'
#' @param data the data object created from running `canning_WIN_report_data`.
#'
#' @param surface colour for surface plots. Can be named colour or hex format.
#'
#' @param bottom colour for bottom plots. Can be named colour or hex format.
#'
#' @return a separate panel plot for each nutrient and a csv of metrics for
#'     inclusion to a table.
#'
#'  @example
#' \dontrun{
#' ann_rep_grp3_c(outpath, data, surface = "blue", bottom = "red")}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @import tidyverse
#' @import lubridate
#' @import ggthemes
#' @import grid
#' @import gridExtra
#'
#' @export

ann_rep_grp3_c <- function(outpath, data, surface, bottom){
  # create output folders paths
  panels <- file.path(outpath, "c_panels")
  tables <- file.path(outpath, "c_tables")

  ##grp3
  grp3 <- c("o_do_in_situ_mg_l", "salinity_mg_l",
            "temperature_in_situ_deg_c", "p_h_no_units")

  # data for grp3 for plotting
  dfgrp3 <- data %>%
    dplyr::filter(emz != "nrz" & collection_method == "Insitu" &
                    sample_type != "Standard") %>%
    dplyr::mutate(salinity_mg_l = salinity_mg_l/1000) %>%
    tidyr::gather(xvar, value, grp3) %>%
    dplyr::group_by(emz, pord, sample_type, rep_per, xvar) %>%
    dplyr::summarise(lower = quantile(value, probs = 0.10, na.rm = TRUE),
                     upper = quantile(value, probs = 0.90, na.rm = TRUE),
                     median = median(value, na.rm = TRUE)) %>%
    dplyr::mutate(filt = paste(stringr::str_sub(emz, 1, 1),
                               stringr::str_to_lower(stringr::str_sub(sample_type, 1, 1)),
                               stringr::str_sub(rep_per, 1, 1),
                               sep = "."))

  # data for same 7 for table summaries only present period
  dfgrp3tbl <- data %>%
    dplyr::filter(emz != "nrz" & collection_method == "Insitu" &
                    sample_type != "Standard", rep_per == "present") %>%

    dplyr::mutate(salinity_mg_l = salinity_mg_l/1000) %>%
    tidyr::gather(xvar, value, grp3) %>%
    dplyr::select(emz, pord, sample_type, xvar, value, mth)

  # mathy labels for plotting
  labgrp3names <- list(c(bquote("DO (mg  "*L^-1*")"), "DO"),
                       c("Salinity (ppt)", "Salinity"),
                       c(bquote("Temperature ("*degree*"C)"), "Temperature"),
                       c("pH", "pH"))

  savenames <- c("DO", "Salinity", "Temperature", "pH")

  csv_out <- data.frame(stringsAsFactors = FALSE)

  for(i in seq_along(grp3)){

    dfgrp3.i <- dplyr::filter(dfgrp3, xvar == grp3[i])

    dfgrp3tbl.i <- dplyr::filter(dfgrp3tbl, xvar == grp3[i]) %>%
      dplyr::mutate(
        sample = factor(case_when(
          sample_type == "Bottom sample" ~ "Bottom",
          sample_type == "Surface sample" ~ "Surface"
        ), levels = c("Surface", "Bottom")),
        emz = factor(emz, levels = c("estuary", "river"))) %>%
      dplyr::group_by(emz, sample, pord) %>%
      dplyr::summarise(n = n(),
                       min = min(value),
                       max = max(value),
                       median = median(value)) %>%
      dplyr::mutate(
        mth = factor(case_when(
          pord == 1 ~ "Jun",
          pord == 2 ~ "Jul",
          pord == 3 ~ "Aug",
          pord == 4 ~ "Sep",
          pord == 5 ~ "Oct",
          pord == 6 ~ "Nov",
          pord == 7 ~ "Dec",
          pord == 8 ~ "Jan",
          pord == 9 ~ "Feb",
          pord == 10 ~ "Mar",
          pord == 11 ~ "Apr",
          pord == 12 ~ "May"
        ), levels = c("Jun", "Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb",
                      "Mar", "Apr","May"))) %>%
      dplyr::select(-pord) %>%
      tidyr::gather("metric", "value", 3:6) %>%
      dplyr::mutate(metric = factor(metric, levels = c("n", "min", "max", "median")),
                    variable = grp3[i]) %>%
      tidyr::spread(mth, value)

    csv_out <- dplyr::bind_rows(csv_out, dfgrp3tbl.i)

    # y limits finder
    dyn_ylim <- dynamic_ylim(dfgrp3.i)
    dyn_ylim_pH <- c(6.5, 9.0)

    dyn_ylim <- if(dfgrp3.i$xvar[1] == "p_h_no_units") dyn_ylim_pH else dyn_ylim

    a <- ggplot(data = dplyr::filter(dfgrp3.i, filt == "e.s.b"), aes(pord)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = surface, alpha = 0.1) +
      geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
      geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
      geom_line(aes(x = pord, y = median), colour = surface) +
      geom_point(data = dplyr::filter(dfgrp3.i, filt == "e.s.p"), aes(x = pord, y = median), pch = 1, size = 1) +
      labs(y = labgrp3names[[i]][[1]]) +
      scale_y_continuous(limits = dyn_ylim) +
      scale_x_continuous(name = bquote((a) ~.(labgrp3names[[i]][[2]]) ~ Canning ~ Estuary ~(Surface)),
                         breaks = c(8:12, 1:7),
                         labels = month.abb[1:12]) +
      theme_bw() +
      theme(panel.grid.major = element_line(linetype = "blank"),
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
            axis.text.x = element_text(size = 6),
            axis.title.y = element_text(size = 9),
            axis.text.y = element_text(size = 7))

    c <- ggplot(data = dplyr::filter(dfgrp3.i, filt == "r.s.b"), aes(pord)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = surface, alpha = 0.1) +
      geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
      geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
      geom_line(aes(x = pord, y = median), colour = surface) +
      geom_point(data = dplyr::filter(dfgrp3.i, filt == "r.s.p"), aes(x = pord, y = median), pch = 1, size = 1) +
      labs(y = labgrp3names[[i]][[1]]) +
      scale_y_continuous(limits = dyn_ylim) +
      scale_x_continuous(name = bquote((c) ~ .(labgrp3names[[i]][[2]]) ~ Lower ~ Canning ~ River ~(Surface)),
                         breaks = c(8:12, 1:7),
                         labels = month.abb[1:12]) +
      theme_bw() +
      theme(panel.grid.major = element_line(linetype = "blank"),
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
            axis.text.x = element_text(size = 6),
            axis.title.y = element_text(size = 9),
            axis.text.y = element_text(size = 7))

    c_leg <- ggplot(data = dplyr::filter(dfgrp3.i, filt == "r.s.b"), aes(pord)) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = surface), alpha = 0.1) +
      scale_fill_manual(name = "",
                        values = surface,
                        labels = "Monthly background 10th and 90th percentile") +
      geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = surface, alpha = 0.4) +
      geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = surface, alpha = 0.4) +
      geom_line(aes(x = pord, y = median, colour = surface)) +
      scale_colour_manual(name = "",
                          values = surface,
                          labels = "Monthly background Median") +
      geom_point(data = dplyr::filter(dfgrp3.i, filt == "r.s.p"), aes(x = pord, y = median, shape = filt),
                 size = 2) +
      scale_shape_manual(name = "",
                         values = 1,
                         labels = "18/19 monthly Median") +
      labs(y = labgrp3names[[i]][[1]]) +
      scale_y_continuous(limits = dyn_ylim) +
      scale_x_continuous(name = "",
                         breaks = c(8:12, 1:7),
                         labels = month.abb[1:12]) +
      theme_bw() +
      theme(legend.position = "bottom", legend.box = "vertical",
            legend.box.just = "left",
            legend.spacing.y = unit(2, 'mm'),
            legend.spacing.x = unit(2, 'mm'),
            legend.key.size = unit(3, "mm"),
            panel.grid.major = element_line(linetype = "blank"),
            panel.grid.minor = element_line(linetype = "blank")) +
      guides(fill = guide_legend(order = 1),
             colour = guide_legend(order = 2),
             shape = guide_legend(order = 3))

    legend_surf <- get_legend(c_leg)

    b <- ggplot(data = dplyr::filter(dfgrp3.i, filt == "e.b.b"), aes(pord)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = bottom, alpha = 0.1) +
      geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = bottom, alpha = 0.4) +
      geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = bottom, alpha = 0.4) +
      geom_line(aes(x = pord, y = median), colour = bottom) +
      geom_point(data = dplyr::filter(dfgrp3.i, filt == "e.b.p"), aes(x = pord, y = median), size = 1) +
      labs(y = labgrp3names[[i]][[1]]) +
      scale_y_continuous(limits = dyn_ylim) +
      scale_x_continuous(name = bquote((b) ~ .(labgrp3names[[i]][[2]]) ~ Canning ~ Estuary ~(Bottom)),
                         breaks = c(8:12, 1:7),
                         labels = month.abb[1:12]) +
      theme_bw() +
      theme(panel.grid.major = element_line(linetype = "blank"),
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
            axis.text.x = element_text(size = 6),
            axis.title.y = element_text(size = 9),
            axis.text.y = element_text(size = 7))

    d <- ggplot(data = dplyr::filter(dfgrp3.i, filt == "r.b.b"), aes(pord)) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = bottom, alpha = 0.1) +
      geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = bottom, alpha = 0.4) +
      geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = bottom, alpha = 0.4) +
      geom_line(aes(x = pord, y = median), colour = bottom) +
      geom_point(data = dplyr::filter(dfgrp3.i, filt == "r.b.p"), aes(x = pord, y = median), size = 1) +
      labs(y = labgrp3names[[i]][[1]]) +
      scale_y_continuous(limits = dyn_ylim) +
      scale_x_continuous(name = bquote((d) ~ .(labgrp3names[[i]][[2]]) ~ Lower ~ Canning ~ River ~(Bottom)),
                         breaks = c(8:12, 1:7),
                         labels = month.abb[1:12]) +
      theme_bw() +
      theme(panel.grid.major = element_line(linetype = "blank"),
            panel.grid.minor = element_line(linetype = "blank"),
            axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
            axis.text.x = element_text(size = 6),
            axis.title.y = element_text(size = 9),
            axis.text.y = element_text(size = 7))

    d_leg <- ggplot(data = dplyr::filter(dfgrp3.i, filt == "r.b.b"), aes(pord)) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = bottom), alpha = 0.1) +
      scale_fill_manual(name = "",
                        values = bottom,
                        labels = "Monthly background 10th-90th percentile") +
      geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = bottom, alpha = 0.4) +
      geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = bottom, alpha = 0.4) +
      geom_line(aes(x = pord, y = median, colour = bottom)) +
      scale_colour_manual(name = "",
                          values = bottom,
                          labels = "Monthly background Median") +
      geom_point(data = dplyr::filter(dfgrp3.i, filt == "r.b.p"), aes(x = pord, y = median, shape = filt),
                 size = 2) +
      scale_shape_manual(name = "",
                         values = 16,
                         labels = "18/19 monthly Median") +
      labs(y = labgrp3names[[i]][[1]]) +
      scale_y_continuous(limits = dyn_ylim) +
      scale_x_continuous(name = "",
                         breaks = c(8:12, 1:7),
                         labels = month.abb[1:12]) +
      theme_bw() +
      theme(legend.position = "bottom", legend.box = "vertical",
            legend.box.just = "left",
            legend.spacing.y = unit(2, 'mm'),
            legend.spacing.x = unit(2, 'mm'),
            legend.key.size = unit(3, "mm"),
            panel.grid.major = element_line(linetype = "blank"),
            panel.grid.minor = element_line(linetype = "blank")) +
      guides(fill = guide_legend(order = 1),
             colour = guide_legend(order = 2),
             shape = guide_legend(order = 3))

    legend_bott <- get_legend(d_leg)

    out <- gridExtra::grid.arrange(a, b, c, d, legend_surf, legend_bott, ncol = 2)

    outname <- paste0(panels, "/", savenames[i], "_panel_grp3_canning.pdf")

    gout <- gtable::gtable_add_padding(out, padding = unit(c(1,2,10,2), "cm"))
    ggsave(filename = outname, plot = gout, width = 10, height = 14,
           units = "in", useDingbats=FALSE)
  }
  csvname <- paste0(tables, "/output_tables_grp3_canning.csv")
  write_csv(csv_out, path = csvname)
}

#' Function for annual Canning River report plots and tables for chlorophyl a.
#'
#' \code{ann_rep_chla_c} takes an export filepath, data, surface and integrated
#'     plot colours and produces panel plots and tables for chlorophyl a.
#'
#' @details Outputs will be exported to two folders  at the outpath
#'     location. `c_panels/` for plots and `c_tables/` for data tables. These
#'     will need to be created if they don't exist.
#'
#' @param outpath filepath to desired export location.
#'
#' @param data the data object created from running `canning_WIN_report_data`.
#'
#' @param surface colour for surface plots. Can be named colour or hex format.
#'
#' @param chloro colour for integrated plots. Can be named colour or hex format.
#'
#' @return a  panel plot for chlorophyl a and a csv of metrics for
#'     inclusion to a table.
#'
#'  @example
#' \dontrun{
#' ann_rep_chla_c(outpath, data, surface = "blue", chloro = "darkgreen")}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @import tidyverse
#' @import lubridate
#' @import ggthemes
#' @import grid
#' @import gridExtra
#'
#' @export

ann_rep_chla_c <- function(outpath, data, surface, chloro){
  # create output folders paths
  panels <- file.path(outpath, "c_panels")
  tables <- file.path(outpath, "c_tables")

  # data for chla for plotting
  dfchla <- data %>%
    dplyr::filter(emz != "nrz" & collection_method == "Grab sample" | collection_method == "Integrated over depth") %>%
    dplyr::filter(sample_type != "Bottom sample") %>%
    tidyr::gather(xvar, value, chlorophyll_a_by_vol_mg_l) %>%
    dplyr::mutate(value = value * 1000) %>%
    dplyr::group_by(emz, pord, sample_type, rep_per, xvar) %>%
    dplyr::summarise(lower = quantile(value, probs = 0.10, na.rm = TRUE),
                     upper = quantile(value, probs = 0.90, na.rm = TRUE),
                     median = median(value, na.rm = TRUE)) %>%
    dplyr::mutate(filt = paste(stringr::str_sub(emz, 1, 1),
                               stringr::str_to_lower(stringr::str_sub(sample_type, 1, 1)),
                               stringr::str_sub(rep_per, 1, 1),
                               sep = "."))

  # mathy labels for plotting
  labchlanames <- list(c(bquote("Chl a ("*mu*L^-1*")"), "Chl a"))

  savenames <- "Chla"

  # data for same chla for table summaries only present period
  dfchlatbl <- data %>%
    dplyr::filter(emz != "nrz" & collection_method == "Grab sample" | collection_method == "Integrated over depth") %>%
    dplyr::filter(sample_type != "Bottom sample", rep_per == "present") %>%
    tidyr::gather(xvar, value, chlorophyll_a_by_vol_mg_l) %>%
    dplyr::mutate(value = value * 1000) %>%
    dplyr::select(emz, pord, sample_type, xvar, value, mth) %>%
    dplyr::mutate(
      sample = factor(case_when(
        sample_type == "Surface sample" ~ "Surface",
        sample_type == "Standard" ~ "Integrated"
      ), levels = c("Surface", "Integrated"))) %>%
    dplyr::group_by(emz, sample, pord) %>%
    dplyr::summarise(n = n(),
                     min = min(value, na.rm = TRUE),
                     max = max(value, na.rm = TRUE),
                     median = median(value, na.rm = TRUE)) %>%
    dplyr::mutate(
      mth = factor(case_when(
        pord == 1 ~ "Jun",
        pord == 2 ~ "Jul",
        pord == 3 ~ "Aug",
        pord == 4 ~ "Sep",
        pord == 5 ~ "Oct",
        pord == 6 ~ "Nov",
        pord == 7 ~ "Dec",
        pord == 8 ~ "Jan",
        pord == 9 ~ "Feb",
        pord == 10 ~ "Mar",
        pord == 11 ~ "Apr",
        pord == 12 ~ "May"
      ), levels = c("Jun", "Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb",
                    "Mar", "Apr","May"))) %>%
    dplyr::select(-pord) %>%
    tidyr::gather("metric", "value", 3:6) %>%
    dplyr::mutate(metric = factor(metric, levels = c("n", "min", "max", "median")),
                  variable = "chlorophyll_a_by_vol_mg_l") %>%
    dplyr::mutate(value = case_when(
      value < 0.5 ~ "<0.5",
      TRUE ~ as.character(value)
    )) %>%
    tidyr::spread(mth, value)

  csv_out <- dfchlatbl

  # y limits finder
  dyn_ylim <- dynamic_ylim(dfchla)

  a <- ggplot(data = dplyr::filter(dfchla, filt == "e.s.b" & sample_type == "Surface sample"),
              aes(pord)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = surface, alpha = 0.1) +
    geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = median), colour = surface) +
    geom_point(data = dplyr::filter(dfchla, filt == "e.s.p"& sample_type == "Surface sample"),
               aes(x = pord, y = median), pch = 1, size = 1) +
    labs(y = labchlanames[[1]][[1]]) +
    scale_y_continuous(limits = dyn_ylim) +
    scale_x_continuous(name = bquote((a) ~.(labchlanames[[1]][[2]]) ~ Canning ~ Estuary ~(Surface)),
                       breaks = c(8:12, 1:7),
                       labels = month.abb[1:12]) +
    theme_bw() +
    theme(panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank"),
          axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
          axis.text.x = element_text(size = 6),
          axis.title.y = element_text(size = 9),
          axis.text.y = element_text(size = 7))

  c <- ggplot(data = dplyr::filter(dfchla, filt == "r.s.b" & sample_type == "Surface sample"),
              aes(pord)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = surface, alpha = 0.1) +
    geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = median), colour = surface) +
    geom_point(data = dplyr::filter(dfchla, filt == "r.s.p" & sample_type == "Surface sample"),
               aes(x = pord, y = median), pch = 1, size = 1) +
    labs(y = labchlanames[[1]][[1]]) +
    scale_y_continuous(limits = dyn_ylim) +
    scale_x_continuous(name = bquote((c) ~ .(labchlanames[[1]][[2]]) ~ Lower ~ Canning ~ River ~(Surface)),
                       breaks = c(8:12, 1:7),
                       labels = month.abb[1:12]) +
    theme_bw() +
    theme(panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank"),
          axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
          axis.text.x = element_text(size = 6),
          axis.title.y = element_text(size = 9),
          axis.text.y = element_text(size = 7))

  c_leg <- ggplot(data = dplyr::filter(dfchla, filt == "r.s.b" & sample_type == "Surface sample"),
                  aes(pord)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = surface), alpha = 0.1) +
    scale_fill_manual(name = "",
                      values = surface,
                      labels = "Monthly background 10th and 90th percentile") +
    geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = median, colour = surface)) +
    scale_colour_manual(name = "",
                        values = surface,
                        labels = "Monthly background Median") +
    geom_point(data = dplyr::filter(dfchla, filt == "r.s.p" & sample_type == "Surface sample"),
               aes(x = pord, y = median, shape = filt),
               size = 2) +
    scale_shape_manual(name = "",
                       values = 1,
                       labels = "18/19 monthly Median") +
    labs(y = labchlanames[[1]][[1]]) +
    scale_y_continuous(limits = dyn_ylim) +
    scale_x_continuous(name = "",
                       breaks = c(8:12, 1:7),
                       labels = month.abb[1:12]) +
    theme_bw() +
    theme(legend.position = "bottom", legend.box = "vertical",
          legend.box.just = "left",
          legend.spacing.y = unit(2, 'mm'),
          legend.spacing.x = unit(2, 'mm'),
          legend.key.size = unit(3, "mm"),
          panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank")) +
    guides(fill = guide_legend(order = 1),
           colour = guide_legend(order = 2),
           shape = guide_legend(order = 3))

  legend_surf <- get_legend(c_leg)

  b <- ggplot(data = dplyr::filter(dfchla, filt == "e.s.b" & sample_type == "Standard"),
              aes(pord)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = chloro, alpha = 0.1) +
    geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = chloro, alpha = 0.4) +
    geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = chloro, alpha = 0.4) +
    geom_line(aes(x = pord, y = median), colour = chloro) +
    geom_point(data = dplyr::filter(dfchla, filt == "e.s.p" & sample_type == "Standard"),
               aes(x = pord, y = median), size = 1) +
    labs(y = labchlanames[[1]][[1]]) +
    scale_y_continuous(limits = dyn_ylim) +
    scale_x_continuous(name = bquote((b) ~.(labchlanames[[1]][[2]]) ~ Canning ~ Estuary ~(Integrated)),
                       breaks = c(8:12, 1:7),
                       labels = month.abb[1:12]) +
    theme_bw() +
    theme(panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank"),
          axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
          axis.text.x = element_text(size = 6),
          axis.title.y = element_text(size = 9),
          axis.text.y = element_text(size = 7))

  d <- ggplot(data = dplyr::filter(dfchla, filt == "r.s.b" & sample_type == "Standard"), aes(pord)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = chloro, alpha = 0.1) +
    geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = chloro, alpha = 0.4) +
    geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = chloro, alpha = 0.4) +
    geom_line(aes(x = pord, y = median), colour = chloro) +
    geom_point(data = dplyr::filter(dfchla, filt == "r.s.p" & sample_type == "Standard"),
               aes(x = pord, y = median), size = 1) +
    labs(y = labchlanames[[1]][[1]]) +
    scale_y_continuous(limits = dyn_ylim) +
    scale_x_continuous(name = bquote((d) ~ .(labchlanames[[1]][[2]]) ~ Lower ~ Canning ~ River ~(Integrated)),
                       breaks = c(8:12, 1:7),
                       labels = month.abb[1:12]) +
    theme_bw() +
    theme(panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank"),
          axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
          axis.text.x = element_text(size = 6),
          axis.title.y = element_text(size = 9),
          axis.text.y = element_text(size = 7))

  d_leg <- ggplot(data = dplyr::filter(dfchla, filt == "r.s.b" & sample_type == "Standard"),
                  aes(pord)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = chloro), alpha = 0.1) +
    scale_fill_manual(name = "",
                      values = chloro,
                      labels = "Monthly background 10th-90th percentile") +
    geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = chloro, alpha = 0.4) +
    geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = chloro, alpha = 0.4) +
    geom_line(aes(x = pord, y = median, colour = chloro)) +
    scale_colour_manual(name = "",
                        values = chloro,
                        labels = "Monthly background Median") +
    geom_point(data = dplyr::filter(dfchla, filt == "r.s.p" & sample_type == "Standard"),
               aes(x = pord, y = median, shape = filt),
               size = 2) +
    scale_shape_manual(name = "",
                       values = 16,
                       labels = "18/19 monthly Median") +
    labs(y = labchlanames[[1]][[1]]) +
    scale_y_continuous(limits = dyn_ylim) +
    scale_x_continuous(name = "",
                       breaks = c(8:12, 1:7),
                       labels = month.abb[1:12]) +
    theme_bw() +
    theme(legend.position = "bottom", legend.box = "vertical",
          legend.box.just = "left",
          legend.spacing.y = unit(2, 'mm'),
          legend.spacing.x = unit(2, 'mm'),
          legend.key.size = unit(3, "mm"),
          panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank")) +
    guides(fill = guide_legend(order = 1),
           colour = guide_legend(order = 2),
           shape = guide_legend(order = 3))

  legend_bott <- get_legend(d_leg)

  out <- gridExtra::grid.arrange(a, b, c, d, legend_surf, legend_bott, ncol = 2)

  outname <- paste0(panels, "/", savenames[1], "_panel_canning.pdf")

  gout <- gtable::gtable_add_padding(out, padding = unit(c(1,2,9,2), "cm"))
  ggsave(filename = outname, plot = gout, width = 10, height = 14,
         units = "in", useDingbats=FALSE)

  csvname <- paste0(tables, "/output_tables_chla_canning.csv")
  write_csv(csv_out, path = csvname)
}

#' Function for annual Canning River report plots and tables for secchi depth.
#'
#' \code{ann_rep_secchi_c} takes an export filepath, data, and surface
#'     plot colour and produces panel plots and tables for the secchi depth.
#'
#' @details Outputs will be exported to two folders at the outpath
#'     location. `c_panels/` for plots and `c_tables/` for data tables. These
#'     will need to be created if they don't exist.
#'
#' @param outpath filepath to desired export location.
#'
#' @param data the data object created from running `canning_WIN_report_data`.
#'
#' @param surface colour for surface plots. Can be named colour or hex format.
#'
#' @return a  panel plot for secchi depth and a csv of metrics for
#'     inclusion to a table.
#'
#'  @example
#' \dontrun{
#' ann_rep_secchi_c(outpath, data, surface = "blue")}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @import tidyverse
#' @import lubridate
#' @import ggthemes
#' @import grid
#' @import gridExtra
#'
#' @export

ann_rep_secchi_c <- function(outpath, data, surface){
  # create output folders paths
  panels <- file.path(outpath, "c_panels")
  tables <- file.path(outpath, "c_tables")

  ##secchi
  sec <- "secchi_depth_m"

  # data for sec for plotting
  dfsec <- data %>%
    dplyr::filter(emz != "nrz" & collection_method == "Insitu") %>%
    dplyr::filter(sample_type == "Standard") %>%
    tidyr::gather(xvar, value, sec) %>%
    dplyr::group_by(emz, pord, sample_type, rep_per, xvar) %>%
    dplyr::summarise(lower = quantile(value, probs = 0.10, na.rm = TRUE),
                     upper = quantile(value, probs = 0.90, na.rm = TRUE),
                     median = median(value, na.rm = TRUE)) %>%
    dplyr::mutate(filt = paste(stringr::str_sub(emz, 1, 1),
                               stringr::str_to_lower(stringr::str_sub(sample_type, 1, 1)),
                               stringr::str_sub(rep_per, 1, 1),
                               sep = "."))

  # mathy labels for plotting
  labsecnames <- list(c(bquote("Depth (m)"), "Secchi Depth"))

  savenames <- "Secchi"

  # data for same grp for table summaries only present period
  dfsectbl <- data %>%
    dplyr::filter(emz != "nrz" & collection_method == "Insitu") %>%
    dplyr::filter(sample_type == "Standard", rep_per == "present") %>%
    tidyr::drop_na(secchi_depth_m) %>%
    tidyr::gather(xvar, value, sec) %>%
    dplyr::select(emz, pord, xvar, value, mth) %>%
    dplyr::mutate(emz = factor(emz, levels = c("estuary", "river"))) %>%
    dplyr::group_by(emz, pord) %>%
    dplyr::summarise(n = n(),
                     min = min(value, na.rm = TRUE),
                     max = max(value, na.rm = TRUE),
                     median = median(value, na.rm = TRUE)) %>%
    dplyr::mutate(
      mth = factor(case_when(
        pord == 1 ~ "Jun",
        pord == 2 ~ "Jul",
        pord == 3 ~ "Aug",
        pord == 4 ~ "Sep",
        pord == 5 ~ "Oct",
        pord == 6 ~ "Nov",
        pord == 7 ~ "Dec",
        pord == 8 ~ "Jan",
        pord == 9 ~ "Feb",
        pord == 10 ~ "Mar",
        pord == 11 ~ "Apr",
        pord == 12 ~ "May"
      ), levels = c("Jun", "Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb",
                    "Mar", "Apr","May"))) %>%
    dplyr::select(-pord) %>%
    tidyr::gather("metric", "value", 2:5) %>%
    dplyr::mutate(metric = factor(metric, levels = c("n", "min", "max", "median")),
                  variable = "secchi") %>%
    tidyr::spread(mth, value)

  csv_out <- dfsectbl

  # y limits finder
  dyn_ylim <- dynamic_ylim(dfsec)

  a <- ggplot(data = dplyr::filter(dfsec, filt == "e.s.b"), aes(pord)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = surface, alpha = 0.1) +
    geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = median), colour = surface) +
    geom_point(data = dplyr::filter(dfsec, filt == "e.s.p"), aes(x = pord, y = median), pch = 1, size = 1) +
    labs(y = labsecnames[[1]][[1]]) +
    scale_y_continuous(limits = dyn_ylim) +
    scale_x_continuous(name = bquote((a) ~.(labsecnames[[1]][[2]]) ~ Canning ~ Estuary),
                       breaks = c(8:12, 1:7),
                       labels = month.abb[1:12]) +
    theme_bw() +
    theme(panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank"),
          axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
          axis.text.x = element_text(size = 6),
          axis.title.y = element_text(size = 9),
          axis.text.y = element_text(size = 7))

  b <- ggplot(data = dplyr::filter(dfsec, filt == "r.s.b"), aes(pord)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = surface, alpha = 0.1) +
    geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.5, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = median), colour = surface) +
    geom_point(data = dplyr::filter(dfsec, filt == "r.s.p"), aes(x = pord, y = median), pch = 1, size = 1) +
    labs(y = labsecnames[[1]][[1]]) +
    scale_y_continuous(limits = dyn_ylim) +
    scale_x_continuous(name = bquote((b) ~ .(labsecnames[[1]][[2]]) ~ Lower ~ Canning ~ River),
                       breaks = c(8:12, 1:7),
                       labels = month.abb[1:12]) +
    theme_bw() +
    theme(panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank"),
          axis.title.x = element_text(face = "bold", hjust = 0, size = 9),
          axis.text.x = element_text(size = 6),
          axis.title.y = element_text(size = 9),
          axis.text.y = element_text(size = 7))

  b_leg <- ggplot(data = dplyr::filter(dfsec, filt == "r.s.b"), aes(pord)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = surface), alpha = 0.1) +
    scale_fill_manual(name = "",
                      values = surface,
                      labels = "Monthly background 10th and 90th percentile") +
    geom_line(aes(x = pord, y = lower), linetype = "dotdash", size = 0.8, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = upper), linetype = "dotdash", size = 0.8, colour = surface, alpha = 0.4) +
    geom_line(aes(x = pord, y = median, colour = surface)) +
    scale_colour_manual(name = "",
                        values = surface,
                        labels = "Monthly background Median") +
    geom_point(data = dplyr::filter(dfsec, filt == "r.s.p"), aes(x = pord, y = median, shape = filt),
               size = 2) +
    scale_shape_manual(name = "",
                       values = 1,
                       labels = "18/19 monthly Median") +
    labs(y = labsecnames[[1]][[1]]) +
    scale_y_continuous(limits = dyn_ylim) +
    scale_x_continuous(name = "",
                       breaks = c(8:12, 1:7),
                       labels = month.abb[1:12]) +
    theme_bw() +
    theme(legend.position = "bottom", legend.box = "vertical",
          legend.box.just = "left",
          legend.spacing.y = unit(2, 'mm'),
          legend.spacing.x = unit(2, 'mm'),
          legend.key.size = unit(3, "mm"),
          panel.grid.major = element_line(linetype = "blank"),
          panel.grid.minor = element_line(linetype = "blank")) +
    guides(fill = guide_legend(order = 1),
           colour = guide_legend(order = 2),
           shape = guide_legend(order = 3))

  legend_surf <- get_legend(b_leg)

  out <- gridExtra::grid.arrange(a, b, legend_surf, ncol = 1)

  outname <- paste0(panels, "/", savenames[1], "_panel_canning.pdf")

  gout <- gtable::gtable_add_padding(out, padding = unit(c(1,7.5,10,7.5), "cm"))
  ggsave(filename = outname, plot = gout, width = 10, height = 14,
         units = "in", useDingbats=FALSE)

  csvname <- paste0(tables, "/output_tables_secchi_canning.csv")
  write_csv(csv_out, path = csvname)
}

#' Function to create all of the plots and tables for the annual Canning River
#' report.
#'
#' \code{canning_reportR} takes an export filepath, data, and surface
#'     plot colour and produces panel plots and tables for all metrics.
#'
#' @details This is a wrapper function that runs all of the individual functions
#'     to produce all of the plots and tables for the Canning River.
#'     Outputs will be exported to two folders created at the outpath
#'     location. `c_panels/` for plots and `c_tables/` for data tables.
#'
#' @param outpath filepath to desired export location.
#'
#' @param data the data object created from running `canning_WIN_report_data`.
#'
#' @param surface colour for surface plots. Can be named colour or hex format.
#'     Defaults to "blue".
#'
#' @param bottom colour for bottom plots. Can be named colour or hex format.
#'     Defaults to "red".
#'
#' @param chloro colour for integrated plots. Can be named colour or hex format.
#'     Defaults to "darkgreen".
#'
#' @return panel plots for all metrics and a csvs of metrics for
#'     inclusion to tables.
#'
#'  @example
#' \dontrun{
#' canning_reportR(outpath, data, surface = "blue", bottom = "red",
#' chloro = "darkgreen")}
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @import tidyverse
#' @import lubridate
#' @import ggthemes
#' @import grid
#' @import gridExtra
#'
#' @export

canning_reportR <- function(inpath, outpath, surface = "blue",
                            bottom = "red", chloro = "darkgreen"){
  file <- list.files(path = inpath, pattern = "SG-E-CANEST_annual_report", full.names = TRUE)
  data <- read_csv(file = file, guess_max = 10000)

  # create output folders
  panels <- file.path(outpath, "c_panels")
  tables <- file.path(outpath, "c_tables")

  if(!file.exists(panels)){
    dir.create(panels)
  }

  if(!file.exists(tables)){
    dir.create(tables)
  }

  #grp1
  ann_rep_grp1_c(outpath, data, surface, bottom)

  #silica
  ann_rep_si_c(outpath, data, surface)

  #doc
  ann_rep_doc_c(outpath, data, surface)

  #grp3
  ann_rep_grp3_c(outpath, data, surface, bottom)

  #chla
  ann_rep_chla_c(outpath, data, surface, chloro)

  #secchi
  ann_rep_secchi_c(outpath, data, surface)

}
