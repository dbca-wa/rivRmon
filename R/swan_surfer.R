# Main function for creating swan surfer plots

#' Reads in external data, combines with internal datasets and produces Swan
#' River surfer plots
#'
#' \code{swan_surfR} takes a file path to Swan River sonde outputs and creates
#'     a four panel (single column) surfer plot of salinity, dissolved oxygen,
#'     chlorophyll a, and temperature in pdf format. The function creates a
#'     directory called `plots/` in the file path to store the pdf's. Code expects
#'     only 2 excel workbooks for one monitoring run. Note sonde data from EXO
#'     models reads depth from the VPos metric. All other sonde models use a
#'     variant of a depth metric.
#'
#' @details Surfer plots display a
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
#' @param path Character string filepath to location of sonde data xlsx
#'     workbooks.
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
#'    Bridge to site POL. Saved to a directory called `plots/`.
#'
#' @examples
#' \dontrun{
#' swan_surfR(path = "Z:/DEC/MonitoringProgram/Data", ovit = "green", ocav = "red")
#' }
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' For more details see  \url{https://dbca-wa.github.io/rivRmon/index.html}
#' {the rivRmon website}
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
#' @importFrom stats complete.cases
#'
#' @export
#'

swan_surfR <- function(path, ovit, ocav){
  suppressWarnings({
    locations <- data_finder(path, river = "s")
    
    # error handler if too many workbooks present
    if(length(locations) == 2){
      samp_date <- unique(substr(locations, nchar(path)+2, nchar(path)+9))
      # make folder for output
      folder <- file.path(path, "plots")
      if (!file.exists(folder)) {
        dir.create(folder)
      }
      # read in lower data
      lower <- sonde_reader(path = locations[1])
      lower_clean <- lower[stats::complete.cases(lower), ]
      
      # read in upper data
      upper <- sonde_reader(path = locations[2])
      upper_clean <- upper[stats::complete.cases(upper), ]
      
      
      ###### New Site Placemark ################################################
      
      # join and delete and rename prob sites
      samp_data <- dplyr::bind_rows(lower_clean, upper_clean) %>%
        janitor::clean_names() %>%
        dplyr::mutate(site = ifelse(site == "MULB FARM", "MUL",site)) %>%
        dplyr::filter(site != "FP2.1")
      
      # join sites to WQ data
      comb_data <- dplyr::left_join(samp_data, S_sitesdf, by = "site")
      d_reduced <- comb_data %>%
        dplyr::select(site, sal_ppt, do_mg_l, c, chl_ug_l, dep_m,
                      dist_mouth, max_depth)
      
      #### bottom adjust
      daily_depth <- d_reduced %>%
        dplyr::group_by(dist_mouth) %>%
        dplyr::summarise(d_depth = -1*(max(dep_m) + 0.2)) %>%
        dplyr::mutate(dist_mouth = dist_mouth/1000)
      
      S_bottom1 <- S_bottom %>%
        dplyr::left_join(daily_depth, by = c("x" = "dist_mouth")) %>%
        dplyr::mutate(y = case_when(
          !is.na(d_depth) ~ d_depth,
          TRUE ~ y
        )) %>%
        dplyr::select(-d_depth)
      
      S_bottom_nar1 <- S_bottom_nar %>%
        dplyr::left_join(daily_depth, by = c("x" = "dist_mouth")) %>%
        dplyr::mutate(y = case_when(
          !is.na(d_depth) ~ d_depth,
          TRUE ~ y
        )) %>%
        dplyr::select(-d_depth)
      
      
      ###### New Site Placemark ################################################
      
      # set up labels and params to plot
      sparams <- c("Salinity", "Dissolved_Oxygen", "Temperature", "Chlorophyll")
      
      # create interpolations and store in  separate lists
      d_all <- d_reduced[stats::complete.cases(d_reduced),] %>%
        dplyr::mutate(y = -1 * dep_m, x = dist_mouth/1000)
      
      d_nar <- d_reduced[stats::complete.cases(d_reduced),] %>%
        dplyr::mutate(y = -1 * dep_m, x = dist_mouth/1000) %>%
        dplyr::filter(x >= 21)
      
      vals <- c("sal_ppt", "do_mg_l", "c", "chl_ug_l")
      
      # ## Krig framework for all of Swan here for reference
      # krig_list_all <- vector("list", length(vals))
      # names(krig_list_all) <- vals
      
      # # for all krig
      # for(i in seq_along(vals)){
      #   val <- vals[i]
      #   d1all <- d_all[,c("x", "y", val)]
      #   names(d1all)[3] <- "value"
      #   sp::coordinates(d1all) <- ~x + y
      #   
      #   # tpsmodall <- fields::Tps(sp::coordinates(d1all), d1all$value)
      #   # S_grd_all_R <- raster::raster(S_grd_all)
      #   # tps_surfaceall <- raster::interpolate(S_grd_all_R, tpsmodall)
      #   # tps_surfaceall[tps_surfaceall < 0] <- 0.1 # spline unfortunately interps to neg vals
      #   
      #   kmod <- automap::autoKrige(value ~ 1, d1all, S_grd_all, 
      #                              model = c("Ste"))#, "Sph", Exp", "Gau", "Ste"
      #   
      #   krig_r <- raster::raster(kmod$krige_output)
      #   
      #   krig_r_classall <- raster::reclassify(krig_r, reclass_matrices[[i]])
      #   krig_spall <- raster::rasterToPoints(krig_r_classall, spatial = TRUE)
      #   krig_dfall <- data.frame(krig_spall)[-4]# ditch option
      #   names(krig_dfall)[1] <- sparams[i]
      #   #idw1_dfall <- idw1_dfall[-(1:526),] # strip weird close to -0.1 vals
      #   krig_list_all[[i]] <- krig_dfall
      # }
      
      ## based on TPS interps from here
      tps_list_all <- vector("list", length(vals))
      names(tps_list_all) <- vals
      
      tps_list_n_s <- vector("list", length(vals))
      names(tps_list_n_s) <- vals
      
      # for all spline TPS
      for(i in seq_along(vals)){
        val <- vals[i]
        d1all <- d_all[,c("x", "y", val)]
        names(d1all)[3] <- "value"
        sp::coordinates(d1all) <- ~x + y
        
        tpsmodall <- fields::Tps(sp::coordinates(d1all), d1all$value)
        S_grd_all_R <- raster::raster(S_grd_all)
        tps_surfaceall <- raster::interpolate(S_grd_all_R, tpsmodall)
        tps_surfaceall[tps_surfaceall < 0] <- 0.1 # spline unfortunately interps to neg vals
        
        tps_r_classall <- raster::reclassify(tps_surfaceall, reclass_matrices[[i]])
        tps_spall <- raster::rasterToPoints(tps_r_classall, spatial = TRUE)
        tps_dfall <- data.frame(tps_spall)[-4]# ditch option
        names(tps_dfall)[1] <- sparams[i]
        tps_list_all[[i]] <- tps_dfall
      }
      
      # for narrows up TPS
      for(i in seq_along(vals)){
        val <- vals[i]
        d1nar <- d_nar[,c("x", "y", val)]
        names(d1nar)[3] <- "value"
        sp::coordinates(d1nar) <- ~x + y
        
        tpsmodnar <- fields::Tps(coordinates(d1nar), d1nar$value)
        S_grd_nar_R <- raster::raster(S_grd_nar)
        tps_surfacenar <- raster::interpolate(S_grd_nar_R, tpsmodnar)
        tps_surfacenar[tps_surfacenar < 0] <- 0.1 # spline unfortunately interps to neg vals
        
        tps_r_classnar <- raster::reclassify(tps_surfacenar, reclass_matrices[[i]])
        tps_spnar <- raster::rasterToPoints(tps_r_classnar, spatial = TRUE)
        tps_dfnar <- data.frame(tps_spnar)[-4]# ditch option
        names(tps_dfnar)[1] <- sparams[i]
        tps_list_n_s[[i]] <- tps_dfnar
      }
      
      
      # make sample collection points
      samp_locs <- comb_data %>%
        dplyr::mutate(dist_mouth = dist_mouth/1000) %>%
        dplyr::rename(x = dist_mouth, y = dep_m) %>%
        dplyr::select(site, x, y)
      
      site_labs <- S_sitesdf %>%
        dplyr::filter(site != "SRP_RSSA") %>%
        dplyr::filter(site != "BWR10" & site != "KMO" & site != "VIT")
      
      site_labs_upper <- S_sitesdf %>%
        dplyr::filter(site != "SRP_RSSA") %>%
        dplyr::filter(site != "VIT")
      
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
        scale_y_continuous(expand = expansion(mult = c(0, .05))) +
        stat_contour2(data = tps_list_all[[1]], aes(x=x, y=y, z = Salinity),
                      colour = "grey5",
                      breaks = MakeBreaks(binwidth = 2),
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("sal"),
                          guide = guide_legend(reverse=T),
                          limits = as.character(seq(2, 42, 2))) +
        geom_rect(data = rectdf_all, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = S_bottom1,
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
        labs(y = "") +
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
        scale_y_continuous(expand = expansion(mult = c(0, .05))) +
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
        geom_polygon(data = S_bottom1,
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
        scale_y_continuous(expand = expansion(mult = c(0, .05))) +
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
        geom_polygon(data = S_bottom1,
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
        scale_y_continuous(expand = expansion(mult = c(0, .05))) +
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
        geom_polygon(data = S_bottom1,
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
      png_name <- paste0(path, "/plots/", "swan_", ymd(samp_date), "_surfer.png")
      
      # add margin padding coarse but effective
      # surfers_pads <- gtable::gtable_add_padding(surfers_s, padding = unit(c(1,4,3,4), "cm"))
      
      ggsave(plot = grid.draw(surfers_s), filename = pdf_name, width=28, height=18)
      cat(paste0(pdf_name,"\n"))
      png(file = png_name, width = 1500, height = 960, res = 53, bg = "transparent") 
      grid.draw(surfers_s)
      dev.off()
      cat(paste0(png_name,"\n"))
      
      
      ## Narrows Up
      # black out areas for plotting _ recalc as deep FP sites screw up narrows plots
      # if present
      rectdf_nar <- S_blockdf_nar %>%
        anti_join(sites_this_week, by = "site")
      
      # plots
      salPlotZ_s <- ggplot()+
        geom_raster(data = tps_list_n_s[[1]],
                    aes(x=x, y=y, fill = factor(Salinity))) +
        scale_x_continuous(limits = c(20.95,51.6),
                           expand = c(0, 0),
                           breaks = c(25, 30, 35, 40, 45, 50)) +
        scale_y_continuous(breaks = c(-8, -6, -4, -2, 0),
                           expand = expansion(mult = c(0, .05)))+
        stat_contour2(data = tps_list_n_s[[1]],
                      aes(x=x, y=y, z = Salinity),
                      colour = "grey10",
                      breaks = MakeBreaks(binwidth = 2),
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("sal"),
                          guide = guide_legend(reverse=T),
                          limits = as.character(seq(2, 42, 2))) +
        geom_rect(data = rectdf_nar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = S_bottom_nar1,
                     aes(x=x, y=y), fill = "grey90", colour = "grey20") +
        geom_point(data = filter(samp_locs, x >= 21),
                   aes(x = x, y = - y),
                   colour = "black",
                   size = 0.5) +
        geom_text(data = filter(site_labs_upper, dist_mouth/1000 >= 21),
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
        labs(y = "") +
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
        geom_raster(data = tps_list_n_s[[2]],
                    aes(x=x, y=y, fill = factor(Dissolved_Oxygen))) +
        scale_x_continuous(limits = c(20.95,51.6),
                           expand = c(0, 0),
                           breaks = c(25, 30, 35, 40, 45, 50)) +
        scale_y_continuous(breaks = c(-8, -6, -4, -2, 0),
                           expand = expansion(mult = c(0, .05))) +
        stat_contour2(data = tps_list_n_s[[2]],
                      aes(x=x, y=y, z = Dissolved_Oxygen),
                      colour = "grey10",
                      breaks = MakeBreaks(binwidth = 1),
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("do"),
                          guide = guide_legend(reverse=T),
                          limits = as.character(seq(1, 17, 1))) +
        geom_rect(data = rectdf_nar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = S_bottom_nar1,
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
        geom_raster(data = tps_list_n_s[[4]],
                    aes(x=x, y=y, fill = factor(Chlorophyll)),
                    alpha = 0.5) +
        scale_x_continuous(limits = c(20.95,51.6),
                           expand = c(0, 0),
                           breaks = c(25, 30, 35, 40, 45, 50)) +
        scale_y_continuous(breaks = c(-8, -6, -4, -2, 0),
                           expand = expansion(mult = c(0, .05))) +
        stat_contour2(data = tps_list_n_s[[4]],
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
        geom_polygon(data = S_bottom_nar1,
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
        geom_raster(data = tps_list_n_s[[3]],
                    aes(x=x, y=y, fill = factor(Temperature))) +
        scale_x_continuous(limits = c(20.95,51.6),
                           expand = c(0, 0),
                           breaks = c(25, 30, 35, 40, 45, 50)) +
        scale_y_continuous(breaks = c(-8, -6, -4, -2, 0),
                           expand = (mult = c(0, .05)))+
        stat_contour2(data = tps_list_n_s[[3]],
                      aes(x=x, y=y, z = Temperature),
                      colour = "grey10",
                      breaks = MakeBreaks(binwidth = 1),
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("temp"),
                          guide = guide_legend(reverse=T),
                          limits = as.character(seq(11, 33, 1))) +
        geom_rect(data = rectdf_nar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = S_bottom_nar1,
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
      png_nameZs <- paste0(path, "/plots/", "swan_middle_upper_", lubridate::ymd(samp_date), "_surfer.png")
      
      # add margin padding coarse but effective
      # surfersZ_pad <- gtable::gtable_add_padding(surfersZs, padding = unit(c(1,4,3,4), "cm"))
      
      ggsave(plot = grid.draw(surfersZs), filename = pdf_nameZs, width=28, height=18)
      cat(paste0(pdf_nameZs,"\n"))
      png(file = png_nameZs, width = 1500, height = 960, res = 53, bg = "transparent") #, 
      grid.draw(surfersZs)
      dev.off()
      cat(paste0(png_nameZs,"\n"))
      
      
    } else {
      stop(paste0("Function expecting only 2 excel workbooks for one monitoring period, ",
                  "or perhaps you need the alternate surfR function."))
    }
  })
  
}

