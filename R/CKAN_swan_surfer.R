# Main function for creating swan surfer plots CKAN version

#' CKAN version. Reads in external data, combines with internal datasets and produces Swan
#' River surfer plots
#'
#' \code{CKAN_swan_surfR} takes a file path to a Project that has a folder
#'     called "data" that contains Swan River sonde outputs and creates
#'     a four panel (single column) surfer plot of salinity, dissolved oxygen,
#'     chlorophyll a, and temperature in pdf format. Code expects
#'     only 2 excel workbooks for one monitoring run. Note sonde data from EXO
#'     models reads depth from the VPos metric. All other sonde models use a
#'     variant of a depth metric.
#'
#' Surfer plots display a
#'     cross-section of the river where the metrics of interest have been
#'     interpolated between sonde locations. Thin plate spline has
#'     been used for the interpolation.
#'
#'     A river "bottom" is displayed which puts the interpolation in context.
#'     The river bottom has been derived from a combination of historical
#'     maximum depths at sampling locations as well as station points extracted
#'     from a "best" navigation line over the latest corporate bathymetry. See
#'     project documentation for further details.
#'
#' @param path Character string filepath to Project location. Sonde data xlsx
#'     workbooks will be there in a directory within the Project called `data/`.
#'
#' @param ovit Character string ("green", "blue" or "red") indicating
#'     oxygenation plant status at site VIT.
#'
#' @param ocav Character string ("green", "blue" or "red") indicating
#'     oxygenation plant status at site CAV.
#'
#' @return Two pdf format four panel surfer plots of the Swan River. One shows
#'    the full extent of the monitoring run (river mouth to the just beyond the
#'    site POL - Upper Swan Power Lines). The second shows from the Narrows
#'    Bridge to site POL. Saved to Project directory `plots/`.
#'
#' @examples
#' \dontrun{
#' swan_surfR(path = "Z:/DEC/MonitoringProgram", ovit = "green", ocav = "red")
#' }
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' @import dplyr
#' @importFrom janitor clean_names
#' @importFrom raster raster interpolate reclassify rasterToPoints merge
#' @import ggplot2
#' @import scales
#' @import grid
#' @import gtable
#' @import metR
#' @importFrom lubridate ymd
#' @importFrom sp coordinates
#' @import fields
#'
#' @export

CKAN_swan_surfR <- function(path, ovit, ocav){
  suppressWarnings({
    locations <- data_finder(paste0(path, "/data") , river = "s")

    # error handler if too many workbooks present
    if(length(locations) == 2){
      #NOTE altered locations due to new filepath length
      samp_date <- unique(substr(locations, nchar(path) + 7, nchar(path) + 14))
      ## No NEED for this as permanent folder setup in Project
      # make folder for output
      # folder <- file.path(path, "plots")
      # if (!file.exists(folder)) {
      #   dir.create(folder)
      # }

      # read in lower data
      lower <- sonde_reader(path = locations[1])
      lower_clean <- lower[complete.cases(lower), ]

      # read in upper data
      upper <- sonde_reader(path = locations[2])
      upper_clean <- upper[complete.cases(upper), ]

      # join and delete and rename prob sites
      samp_data <- dplyr::bind_rows(lower_clean, upper_clean) %>%
        janitor::clean_names() %>%
        dplyr::mutate(site = ifelse(site == "MULB FARM", "MUL",
                                    ifelse(site == "BWR10", "BWR", site))) %>%
        dplyr::filter(site != "FP2.1")

      # join sites to WQ data
      comb_data <- dplyr::left_join(samp_data, S_sitesdf, by = "site")
      d_reduced <- comb_data %>%
        dplyr::select(site, sal_ppt, do_mg_l, c, chl_ug_l, dep_m,
                      dist_mouth, max_depth)

      # set up labels and params to plot
      sparams <- c("Salinity", "Dissolved_Oxygen", "Temperature", "Chlorophyll")

      # create interpolations and store in  separate lists
      d_all <- d_reduced[complete.cases(d_reduced),] %>%
        dplyr::mutate(y = -1 * dep_m, x = dist_mouth/1000)

      d_nar <- d_reduced[complete.cases(d_reduced),] %>%
        dplyr::mutate(y = -1 * dep_m, x = dist_mouth/1000) %>%
        dplyr::filter(x >= 21)

      vals <- c("sal_ppt", "do_mg_l", "c", "chl_ug_l")

      ## based on TPS interps from here
      tps_list_all <- vector("list", length(vals))
      names(tps_list_all) <- vals

      # for all spline
      for(i in seq_along(vals)){
        val <- vals[i]
        d1all <- d_all[,c("x", "y", val)]
        names(d1all)[3] <- "value"
        sp::coordinates(d1all) <- ~x + y

        tpsmodall <- fields::Tps(sp::coordinates(d1all), d1all$value)
        S_grd_all_R <- raster::raster(S_grd_all)
        tps_surfaceall <- raster::interpolate(S_grd_all_R, tpsmodall)
        tps_surfaceall[tps_surfaceall < 0] <- 0.1 # spline unfortunately interps to neg vals

        idw1_r_classall <- raster::reclassify(tps_surfaceall, reclass_matrices[[i]])
        idw1_spall <- raster::rasterToPoints(idw1_r_classall, spatial = TRUE)
        idw1_dfall <- data.frame(idw1_spall)[-4]# ditch option
        names(idw1_dfall)[1] <- sparams[i]
        #idw1_dfall <- idw1_dfall[-(1:526),] # strip weird close to -0.1 vals
        tps_list_all[[i]] <- idw1_dfall
      }

      # make sample collection points
      samp_locs <- comb_data %>%
        dplyr::mutate(dist_mouth = dist_mouth/1000) %>%
        dplyr::rename(x = dist_mouth, y = dep_m) %>%
        dplyr::select(site, x, y)

      site_labs <- S_sitesdf %>%
        dplyr::filter(site != "SRP_RSSA") %>%
        dplyr::filter(site != "BWR" & site != "KMO" & site != "VIT")

      # construct pretty date
      sday <- just_nums(as.numeric(substr(samp_date, 7, 8)))
      sdate <- paste(sday, format(ymd(samp_date), "%b %Y"), sep = " ")

      # oxygenation plant status colour
      oxy_col <- c(ovit, ocav)
      S_oxy_locs$c <- oxy_col

      # data frame of sites in this run
      sites_this_week <- tibble(site = unique(d_all$site))

      ## All River
      # black out areas for plotting
      rectdf_all <- S_blockdf_all %>%
        anti_join(sites_this_week, by = "site")

      # plots
      salPlot_s <- ggplot()+
        geom_raster(data = tps_list_all[[1]],
                    aes(x=x, y=y, fill = factor(Salinity))) +
        scale_x_continuous(limits = c(-1, 51.6),
                           expand = c(0, 0),
                           breaks = seq(0, 50, by = 5)) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
        stat_contour2(data = tps_list_all[[1]], aes(x=x, y=y, z = Salinity),
                      colour = "grey10",
                      breaks = MakeBreaks(binwidth = 2),
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("sal"),
                          guide = guide_legend(reverse=T),
                          limits = as.character(seq(2, 42, 2))) +
        geom_rect(data = rectdf_all, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = S_bottom,
                     aes(x=x, y=y), fill = "grey90", colour = "grey20") +
        geom_point(data = samp_locs,
                   aes(x = x, y = - y),
                   colour = "black",
                   size = 0.5) +
        geom_text(data = site_labs,
                  aes(x = dist_mouth/1000, y = 0.9, label = site, fontface=2),
                  size = 4.5,
                  colour = "black",
                  alpha = 1,
                  check_overlap = TRUE) +
        annotate("text",
                 label = "Salinity (ppt)",
                 x = 19,
                 y = -16.5,
                 size = 9,
                 fontface =2,
                 colour = "black") +
        labs(title = paste("Swan River Estuary - Physical-Chemical Profile -",
                           sdate),
             y = "") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.ticks.length.y.left = (unit(2, "mm")),
              axis.ticks.length.y.right = (unit(2, "mm")),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust=0.5, vjust=0.5, face='bold', size = 28),
              plot.subtitle = element_text(hjust=0.5, vjust=0.5, size = 22),
              legend.background = element_rect(fill = "transparent"),
              legend.direction = "horizontal",
              legend.position = c(0.65, 0.22),
              legend.key.size =  unit(8, "mm"),
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              plot.margin=grid::unit(c(0,0,0,0), "mm")) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                                   label.position = "bottom"))

      doPlot_s <- ggplot()+
        geom_raster(data = tps_list_all[[2]],
                    aes(x=x, y=y, fill = factor(Dissolved_Oxygen))) +
        scale_x_continuous(limits = c(-1, 51.6),
                           expand = c(0, 0),
                           breaks = seq(0, 50, by = 5)) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
        stat_contour2(data = tps_list_all[[2]],
                      aes(x=x, y=y, z = Dissolved_Oxygen),
                      colour = "grey10",
                      breaks = MakeBreaks(binwidth = 1),
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("do"),
                          guide = guide_legend(reverse=T),
                          limits = as.character(seq(1, 17, 1))) +
        geom_rect(data = rectdf_all, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = S_bottom,
                     aes(x=x, y=y), fill = "grey90", colour = "grey20") +
        geom_point(data = samp_locs,
                   aes(x = x, y = - y),
                   colour = "black",
                   size = 0.5) +
        geom_point(data = S_oxy_locs,
                   aes(x = x, y = y),
                   size = 6,
                   colour = "black",
                   bg = S_oxy_locs$c,
                   shape = 24) +
        annotate("text",
                 label = "Dissolved Oxygen (mg/L)",
                 x = 21,
                 y = -16.7,
                 size = 9,
                 fontface = 2,
                 colour = "black") +
        annotation_custom(grob = oxy_grob, xmin = 40, xmax = 47, ymin = -20, ymax = -12) +
        labs(y = "Depth (m)") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.ticks.length.y.left = (unit(2, "mm")),
              axis.ticks.length.y.right = (unit(2, "mm")),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust=0.5, vjust=0.5, face='bold'),
              plot.subtitle = element_text(hjust=0.5, vjust=0.5),
              legend.background = element_rect(fill = "transparent"),
              legend.direction = "horizontal",
              legend.position = c(0.65, 0.22),
              legend.key.size =  unit(8, "mm"),
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              plot.margin=grid::unit(c(0,0,0,0), "mm")) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                                   label.position = "bottom"))

      chlorPlot_s <- ggplot()+
        geom_raster(data = tps_list_all[[4]],
                    aes(x=x, y=y, fill = factor(Chlorophyll)),
                    alpha = 0.5) +
        scale_x_continuous(limits = c(-1, 51.6),
                           expand = c(0, 0),
                           breaks = seq(0, 50, by = 5)) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
        stat_contour2(data = tps_list_all[[4]],
                      aes(x=x, y=y, z = Chlorophyll),
                      colour = "grey10",
                      breaks = as.numeric(chl_brk),
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("chl"),
                          guide = guide_legend(reverse=T),
                          labels = c("20", "40", "60", "80", "120", "160",
                                     "200", "300", "400", "> 400"),
                          limits = c(as.character(seq(20, 80, 20)),
                                     "120", "160", "200", "300", "400", "1000")) +
        geom_rect(data = rectdf_all, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = S_bottom,
                     aes(x=x, y=y), fill = "grey90", colour = "grey20") +
        geom_point(data = samp_locs,
                   aes(x = x, y = - y),
                   colour = "black",
                   size = 0.5) +
        annotate("text",
                 label = expression('bold(paste("F-Chlorophyll (", mu,"g/L)"))'),
                 x = 20.4,
                 y = -16.8,
                 size = 9,
                 fontface =2,
                 colour = "black", parse = TRUE) +
        labs(x = "Distance From Entrance (km)",
             y = "") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.ticks.length.y.left = (unit(2, "mm")),
              axis.ticks.length.y.right = (unit(2, "mm")),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust=0.5, vjust=0.5, face='bold', size = 24),
              plot.subtitle = element_text(hjust=0.5, vjust=0.5, size = 22),
              legend.background = element_rect(fill = "transparent"),
              legend.direction = "horizontal",
              legend.position = c(0.65, 0.22),
              legend.key.size =  unit(8, "mm"),
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              plot.margin=grid::unit(c(0,0,0,0), "mm")) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                                   label.position = "bottom"))

      tempPlot_s <- ggplot()+
        geom_raster(data = tps_list_all[[3]],
                    aes(x=x, y=y, fill = factor(Temperature))) +
        scale_x_continuous(limits = c(-1, 51.6),
                           expand = c(0, 0),
                           breaks = seq(0, 50, by = 5)) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
        stat_contour2(data = tps_list_all[[3]],
                      aes(x=x, y=y, z = Temperature),
                      colour = "grey10",
                      breaks = MakeBreaks(binwidth = 1),
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("temp"),
                          guide = guide_legend(reverse=T),
                          limits = as.character(seq(11, 33, 1))) +
        geom_rect(data = rectdf_all, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = S_bottom,
                     aes(x=x, y=y), fill = "grey90", colour = "grey20") +
        geom_point(data = samp_locs,
                   aes(x = x, y = - y),
                   colour = "black",
                   size = 0.5) +
        annotate("text",
                 label = expression('bold(paste("Temperature (", degree,"C)"))'),
                 x = 19,
                 y = -16.8,
                 size = 9,
                 fontface =2,
                 colour = "black", parse = TRUE) +
        labs(x = "Distance From Entrance (km)",
             y = "") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.line.y = element_blank(),
              axis.ticks.length.y.left = (unit(2, "mm")),
              axis.ticks.length.y.right = (unit(2, "mm")),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              axis.text.x = element_text(vjust = 0.5),
              plot.title = element_text(hjust = 0.5, vjust = 0.5, face = 'bold', size = 24),
              plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 22),
              legend.background = element_rect(fill = "transparent"),
              legend.direction = "horizontal",
              legend.position = c(0.65, 0.22),
              legend.key.size =  unit(8, "mm"),
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              plot.margin=grid::unit(c(0,0,0,0), "mm")) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                                   label.position = "bottom"))

      # create list of plotGrobs
      plts <- lapply(list(salPlot_s, doPlot_s, chlorPlot_s, tempPlot_s), ggplotGrob)
      # rbind (i.e. 1 column) size arg matters!
      surfers_s <- rbind(plts[[1]], plts[[2]], plts[[3]], plts[[4]], size = "first")
      pdf_name <- paste0(path, "/plots/", "swan_", ymd(samp_date), "_surfer.pdf")
      cat(paste0(pdf_name,"\n"))
      # add margin padding coarse but effective
      surfers_pads <- gtable::gtable_add_padding(surfers_s, padding = unit(c(1,4,3,4), "cm"))

      ggsave(plot = grid.draw(surfers_pads), filename = pdf_name, width=28, height=18)

      ## Narrows Up
      rectdf_nar <- S_blockdf_nar %>%
        anti_join(sites_this_week, by = "site")

      # plots
      salPlotZ_s <- ggplot()+
        geom_raster(data = tps_list_all[[1]][tps_list_all[[1]]$y >= -10.05,],
                    aes(x=x, y=y, fill = factor(Salinity))) +
        scale_x_continuous(limits = c(20.95,51.6),
                           expand = c(0, 0),
                           breaks = c(25, 30, 35, 40, 45, 50)) +
        scale_y_continuous(breaks = c(-8, -6, -4, -2, 0),
                           expand = expand_scale(mult = c(0, .05)))+
        stat_contour2(data = tps_list_all[[1]][tps_list_all[[1]]$y >= -10.05,],
                      aes(x=x, y=y, z = Salinity),
                      colour = "grey10",
                      breaks = MakeBreaks(binwidth = 2),
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("sal"),
                          guide = guide_legend(reverse=T),
                          limits = as.character(seq(2, 42, 2))) +
        geom_rect(data = rectdf_nar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = S_bottom_nar,
                     aes(x=x, y=y), fill = "grey90", colour = "grey20") +
        geom_point(data = filter(samp_locs, x >= 21),
                   aes(x = x, y = - y),
                   colour = "black",
                   size = 0.5) +
        geom_text(data = filter(site_labs, dist_mouth/1000 >= 21),
                  aes(x = dist_mouth/1000, y = 0.7, label = site, fontface=2),
                  size = 4.5,
                  colour = "black",
                  alpha = 1,
                  check_overlap = TRUE) +
        annotate("text",
                 label = "Salinity (ppt)",
                 x = 32.8,
                 y = -7.4,
                 size = 9,
                 fontface =2,
                 colour = "black") +
        labs(title = paste("Middle and Upper Swan River Estuary - Physical-Chemical Profile -",
                           sdate),
             y = "") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.ticks.length.y.left = (unit(2, "mm")),
              axis.ticks.length.y.right = (unit(2, "mm")),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust=0.5, vjust=0.5, face='bold', size = 28),
              plot.subtitle = element_text(hjust=0.5, vjust=0.5, size = 22),
              legend.background = element_rect(fill = "transparent"),
              legend.direction = "horizontal",
              legend.title = element_blank(),
              legend.position = c(0.65, 0.22),
              legend.key.size =  unit(8, "mm"),
              legend.text = element_text(size = 12),
              plot.margin=grid::unit(c(0,0,0,0), "mm")) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                                   label.position = "bottom"))


      doPlotZ_s <- ggplot()+
        geom_raster(data = tps_list_all[[2]][tps_list_all[[2]]$y >= -10.05,],
                    aes(x=x, y=y, fill = factor(Dissolved_Oxygen))) +
        scale_x_continuous(limits = c(20.95,51.6),
                           expand = c(0, 0),
                           breaks = c(25, 30, 35, 40, 45, 50)) +
        scale_y_continuous(breaks = c(-8, -6, -4, -2, 0),
                           expand = expand_scale(mult = c(0, .05))) +
        stat_contour2(data = tps_list_all[[2]][tps_list_all[[2]]$y >= -10.05,],
                      aes(x=x, y=y, z = Dissolved_Oxygen),
                      colour = "grey10",
                      breaks = MakeBreaks(binwidth = 1),
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("do"),
                          guide = guide_legend(reverse=T),
                          limits = as.character(seq(1, 17, 1))) +
        geom_rect(data = rectdf_nar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = S_bottom_nar,
                     aes(x=x, y=y), fill = "grey90", colour = "grey20") +
        geom_point(data = filter(samp_locs, x >= 21),
                   aes(x = x, y = - y),
                   colour = "black",
                   size = 0.5) +
        geom_point(data = S_oxy_locs,
                   aes(x = x, y = y + 1),#nudge up
                   size = 6,
                   colour = "black",
                   bg = S_oxy_locs$c,
                   shape = 24) +
        annotate("text",
                 label = "Dissolved Oxygen (mg/L)",
                 x = 34,
                 y = -7.57,
                 size = 9,
                 fontface = 2,
                 colour = "black") +
        annotation_custom(grob = oxy_grob, xmin = 44, xmax = 50, ymin = -9, ymax = -6.8) +
        labs(y = "Depth (m)") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.ticks.length.y.left = (unit(2, "mm")),
              axis.ticks.length.y.right = (unit(2, "mm")),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust=0.5, vjust=0.5, face='bold'),
              plot.subtitle = element_text(hjust=0.5, vjust=0.5),
              legend.background = element_rect(fill = "transparent"),
              legend.direction = "horizontal",
              legend.position = c(0.65, 0.22),
              legend.key.size =  unit(8, "mm"),
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              plot.margin=grid::unit(c(0,0,0,0), "mm")) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                                   label.position = "bottom"))

      chlorPlotZ_s <- ggplot()+
        geom_raster(data = tps_list_all[[4]][tps_list_all[[4]]$y >= -10.05,],
                    aes(x=x, y=y, fill = factor(Chlorophyll)),
                    alpha = 0.5) +
        scale_x_continuous(limits = c(20.95,51.6),
                           expand = c(0, 0),
                           breaks = c(25, 30, 35, 40, 45, 50)) +
        scale_y_continuous(breaks = c(-8, -6, -4, -2, 0),
                           expand = expand_scale(mult = c(0, .05))) +
        stat_contour2(data = tps_list_all[[4]][tps_list_all[[4]]$y >= -10.05,],
                      aes(x=x, y=y, z = Chlorophyll),
                      colour = "grey10",
                      breaks = as.numeric(chl_brk),
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("chl"),
                          guide = guide_legend(reverse=T),
                          labels = c("20", "40", "60", "80", "120", "160",
                                     "200", "300", "400", "> 400"),
                          limits = c(as.character(seq(20, 80, 20)),
                                     "120", "160", "200", "300", "400", "1000")) +
        geom_rect(data = rectdf_nar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = S_bottom_nar,
                     aes(x=x, y=y), fill = "grey90", colour = "grey20") +
        geom_point(data = filter(samp_locs, x >= 21),
                   aes(x = x, y = - y),
                   colour = "black",
                   size = 0.5) +
        annotate("text",
                 label = expression('bold(paste("F-Chlorophyll (", mu,"g/L)"))'),
                 x = 33.6,
                 y = -7.6,
                 size = 9,
                 fontface =2,
                 colour = "black", parse = TRUE) +
        labs(x = "Distance From Entrance (km)",
             y = "") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.ticks.length.y.left = (unit(2, "mm")),
              axis.ticks.length.y.right = (unit(2, "mm")),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust=0.5, vjust=0.5, face='bold', size = 24),
              plot.subtitle = element_text(hjust=0.5, vjust=0.5, size = 22),
              legend.background = element_rect(fill = "transparent"),
              legend.direction = "horizontal",
              legend.position = c(0.65, 0.22),
              legend.key.size =  unit(8, "mm"),
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              plot.margin=grid::unit(c(0,0,0,0), "mm")) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                                   label.position = "bottom"))

      tempPlotZ_s <- ggplot()+
        geom_raster(data = tps_list_all[[3]][tps_list_all[[3]]$y >= -10.05,],
                    aes(x=x, y=y, fill = factor(Temperature))) +
        scale_x_continuous(limits = c(20.95,51.6),
                           expand = c(0, 0),
                           breaks = c(25, 30, 35, 40, 45, 50)) +
        scale_y_continuous(breaks = c(-8, -6, -4, -2, 0),
                           expand = expand_scale(mult = c(0, .05)))+
        stat_contour2(data = tps_list_all[[3]][tps_list_all[[3]]$y >= -10.05,],
                      aes(x=x, y=y, z = Temperature),
                      colour = "grey10",
                      breaks = MakeBreaks(binwidth = 1),
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("temp"),
                          guide = guide_legend(reverse=T),
                          limits = as.character(seq(11, 33, 1))) +
        geom_rect(data = rectdf_nar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = S_bottom_nar,
                     aes(x=x, y=y), fill = "grey90", colour = "grey20") +
        geom_point(data = filter(samp_locs, x >= 21),
                   aes(x = x, y = - y),
                   colour = "black",
                   size = 0.5) +
        annotate("text",
                 label = expression('bold(paste("Temperature (", degree,"C)"))'),
                 x = 33,
                 y = -7.6,
                 size = 9,
                 fontface =2,
                 colour = "black", parse = TRUE) +
        labs(x = "Distance From Entrance (km)",
             y = "") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.line.y = element_blank(),
              axis.ticks.length.y.left = (unit(2, "mm")),
              axis.ticks.length.y.right = (unit(2, "mm")),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              axis.text.x = element_text(vjust = 0.5),
              plot.title = element_text(hjust = 0.5, vjust = 0.5, face = 'bold', size = 24),
              plot.subtitle = element_text(hjust = 0.5, vjust = 0.5, size = 22),
              legend.background = element_rect(fill = "transparent"),
              legend.direction = "horizontal",
              legend.position = c(0.65, 0.22),
              legend.key.size =  unit(8, "mm"),
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              plot.margin=grid::unit(c(0,0,0,0), "mm")) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                                   label.position = "bottom"))


      # for zoomed at narrows
      # create list of plotGrobs
      pltzs <- lapply(list(salPlotZ_s, doPlotZ_s, chlorPlotZ_s, tempPlotZ_s), ggplotGrob)
      # rbind (i.e. 1 column) size arg matters!
      surfersZs <- rbind(pltzs[[1]], pltzs[[2]], pltzs[[3]], pltzs[[4]], size = "first")
      pdf_nameZs <- paste0(path, "/plots/", "swan_middle_upper_", lubridate::ymd(samp_date), "_surfer.pdf")
      cat(paste0(pdf_nameZs,"\n"))
      # add margin padding coarse but effective
      surfersZ_pad <- gtable::gtable_add_padding(surfersZs, padding = unit(c(1,4,3,4), "cm"))

      ggsave(plot = grid.draw(surfersZ_pad), filename = pdf_nameZs, width=28, height=18)

    } else {
      stop(paste0("Function expecting only 2 excel workbooks for one monitoring period, ",
                  "or perhaps you need the alternate surfR function."))
    }
  })

}
