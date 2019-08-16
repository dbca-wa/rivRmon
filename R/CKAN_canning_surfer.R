# Main function for creating canning surfer plots

#' Reads in external data, combines with internal datasets and produces Canning
#' River surfer plots
#'
#' \code{canning_surfR} takes a file path to Canning River sonde output and creates
#'     a four panel (single column) surfer plot of salinity, dissolved oxygen,
#'     chlorophyll a, and temperature in pdf format. The function creates a
#'     directory called `plots/` in the file path to store the pdf's. Code expects
#'     only 2 excel workbooks for one monitoring run.  Note sonde data from EXO
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
#' @param obac Character string ("green", "blue" or "red") indicating
#'     oxygenation plant status at site BAC.
#'
#' @param onic Character string ("green", "blue" or "red") indicating
#'     oxygenation plant status at site NIC.
#'
#' @return A pdf format four panel surfer plot of the Canning River. Saved to
#'     a directory called `plots/`.
#'
#' @examples
#' \dontrun{
#' canning_surfR(path = "Z:/DEC/MonitoringProgram/Data", obac = "green", onic = "red")
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

CKAN_canning_surfR <- function(path, obac, onic){
  suppressWarnings({
    locations <- data_finder(paste0(path, "/data"), river = "c")

    #error handler
    if(length(locations) == 2){
      samp_date <- unique(substr(locations, nchar(path) + 7, nchar(path) + 14))
      # # make folder for output
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
        janitor::clean_names()

      # join sites to WQ data
      comb_data <- dplyr::left_join(samp_data, C_sitesdf, by = "site")
      d_reduced <- comb_data %>%
        dplyr::select(site, sal_ppt, do_mg_l, c, chl_ug_l, dep_m,
                      dist_bridg, max_depth)
      # Set up labels and params to plot
      sparams <- c("Salinity", "Dissolved_Oxygen", "Temperature", "Chlorophyll")

      # filter sampling data for separate interpolations
      d_all <- d_reduced[complete.cases(d_reduced),] %>%
        dplyr::mutate(y = -1 * dep_m, x = dist_bridg/1000)

      d_low <- d_reduced[complete.cases(d_reduced),] %>%
        dplyr::mutate(y = -1 * dep_m, x = dist_bridg/1000) %>%
        dplyr::filter(x < 11.3)

      d_up <- d_reduced[complete.cases(d_reduced),] %>%
        dplyr::mutate(y = -1 * dep_m, x = dist_bridg/1000) %>%
        dplyr::filter(x > 11.3)

      # Based on TPS interps from here
      vals <- c("sal_ppt", "do_mg_l", "c", "chl_ug_l")
      tps_list_all <- vector("list", length(vals))
      tps_list_weir <- vector("list", length(vals))
      names(tps_list_all) <- vals
      names(tps_list_weir) <- vals #will combine upper and lower here


      # for all TPS
      for(i in seq_along(vals)){
        val <- vals[i]
        d1 <- d_all[,c("x", "y", val)]
        names(d1)[3] <- "value"
        sp::coordinates(d1) <- ~x + y

        tpsmodall <- fields::Tps(sp::coordinates(d1), d1$value)
        C_grd_all_R <- raster::raster(C_grd_all)
        tps_surfaceall <- raster::interpolate(C_grd_all_R, tpsmodall)
        tps_surfaceall[tps_surfaceall < 0] <- 0.1 # spline unfortunately interps to neg vals

        tps_r_classall <- raster::reclassify(tps_surfaceall, reclass_matrices[[i]])
        tps_points <- raster::rasterToPoints(tps_r_classall, spatial = TRUE)
        tps_df <- data.frame(tps_points)[-4]  # ditch option
        names(tps_df)[1] <- sparams[i]
        tps_list_all[[i]] <- tps_df
      }

      # for weir
      for(i in seq_along(vals)){
        val <- vals[i]
        d1 <- d_low[,c("x", "y", val)]
        d2 <- d_up[,c("x", "y", val)]
        names(d1)[3] <- "value"
        names(d2)[3] <- "value"
        sp::coordinates(d1) <- ~x + y
        sp::coordinates(d2) <- ~x + y

        # for lower
        tpsmodlc <- fields::Tps(sp::coordinates(d1), d1$value)
        C_grd_low_R <- raster::raster(C_grd_low)
        tps_surfacelc <- raster::interpolate(C_grd_low_R, tpsmodlc)
        tps_surfacelc[tps_surfacelc < 0] <- 0.1

        # for upper
        tpsmoduc <- fields::Tps(sp::coordinates(d2), d2$value)
        C_grd_up_R <- raster::raster(C_grd_up)
        tps_surfaceuc <- raster::interpolate(C_grd_up_R, tpsmoduc)
        tps_surfaceuc[tps_surfaceuc <- 0] <- 0.1

        # make one raster
        comb_tps_surface <- raster::merge(tps_surfacelc, tps_surfaceuc)

        # reclassify
        tps_r_class_comb <- raster::reclassify(comb_tps_surface, reclass_matrices[[i]])
        tps_points <- raster::rasterToPoints(tps_r_class_comb, spatial = TRUE)
        tps_df <- data.frame(tps_points)[-4]  # ditch option
        names(tps_df)[1] <- sparams[i]
        # tps_df <- tps_df[-(1:769),] # strip weird close to -0.1 vals
        tps_list_weir[[i]] <- tps_df
      }

      # make sample collection points
      samp_locs <- comb_data %>%
        dplyr::mutate(dist_bridg = dist_bridg/1000) %>%
        dplyr::rename(x = dist_bridg, y = dep_m) %>%
        dplyr::select(site, x, y)

      samp_labels <- c("SCB2", "SAL", "RIV", "CASMID", "KEN", "BAC", "NIC", "ELL")
      samp_labels_locs <- C_sitesdf %>%
        dplyr::filter(site %in% samp_labels)


      # Logic for weir no weir
      cannoxy <- c("KENU300", "BACD500", "BACD300", "BACU300", "PO2", "GRE", "KS7",
                   "MASD50", "NICD200", "KS9", "PAC", "MACD50")
      if(sum(cannoxy %in% samp_locs$site) > 0 ){
        bottom <- C_bottom_weir
        interp <- tps_list_weir
        C_blockdf <- C_blockdf_weir
      } else {
        bottom <- C_bottom_open
        interp <- tps_list_all
        C_blockdf <- C_blockdf_all
      }

      # construct pretty date
      sday <- just_nums(as.numeric(substr(samp_date, 7, 8)))
      sdate <- paste(sday, format(ymd(samp_date), "%b %Y"), sep = " ")

      # oxy cols
      oxy_col <- c(obac, onic)
      C_oxy_locs$c <- oxy_col

      # data frame of sites in this run
      sites_this_week <- tibble(site = unique(d_all$site))

      # black out areas for plotting
      rectdf <- C_blockdf %>%
        anti_join(sites_this_week, by = "site")


      salPlot <- ggplot()+
        geom_tile(data = interp[[1]],
                  aes(x=x, y=y, fill = factor(Salinity))) +
        scale_x_continuous(limits = c(0.5, 15.95),
                           expand = c(0, 0),
                           breaks = seq(0, 14, by = 2)) +
        scale_y_continuous(breaks = c(0, -2, -4, -6),
                           expand = expand_scale(mult = c(0, 0.05))) +
        stat_contour2(data = interp[[1]], aes(x=x, y=y, z = Salinity),
                      colour = "grey10",
                      breaks = MakeBreaks(binwidth = 2),
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("sal"),
                          guide = guide_legend(reverse=T),
                          limits = as.character(seq(2, 42, 2))) +
        # geom_text_contour(data = interp[[1]],
        #                   aes(x=x, y=y, z = Salinity),
        #                   #skip = 1,
        #                   size = 6,
        #                   check_overlap = TRUE,
        #                   #stroke = 0.2,
        #                   breaks = MakeBreaks(binwidth = 2),
        #                   nudge_y = 0.5) +
        geom_rect(data = rectdf, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = bottom,
                     aes(x=x, y=y), fill = "grey90", colour = "grey20") +
        geom_point(data = samp_locs,
                   aes(x = x, y = - y),
                   colour = "black",
                   size = 0.5) +
        geom_text(data = samp_labels_locs,
                  aes(x = dist_bridg/1000, y = 0.7, label = site, fontface = 2),
                  size = 4.5,
                  colour = "black",
                  alpha = 1,
                  check_overlap = TRUE) +
        annotate("text",
                 label = "Salinity (ppt)",
                 x = 3.25,
                 y = -5.65,
                 size = 9,
                 fontface = 2,
                 colour = "black") +
        labs(title = paste("Canning River Estuary - Physical-Chemical Profile -",
                           sdate),
             y = "") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border=element_blank(),
              axis.line = element_line(colour = "black"),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.ticks.length.y.left = (unit(2, "mm")),
              axis.ticks.length.y.right = (unit(2, "mm")),
              #axis.ticks.x = element_blank(),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust=0.5, vjust=0.5, face='bold', size = 28),
              plot.subtitle = element_text(hjust=0.5, vjust=0.5, size = 22),
              legend.background = element_rect(fill = "transparent"),
              legend.direction = "horizontal",
              legend.position = c(0.43, 0.15),
              legend.key.size =  unit(8, "mm"),
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              plot.margin=grid::unit(c(0,0,0,0), "mm")) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                                   label.position = "bottom"))

      doPlot <- ggplot()+
        geom_tile(data = interp[[2]],
                  aes(x=x, y=y, fill = factor(Dissolved_Oxygen))) +
        scale_x_continuous(limits = c(0.5, 15.95),
                           expand = c(0, 0),
                           breaks = seq(0, 14, by = 2)) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
        stat_contour2(data = interp[[2]],
                      aes(x=x, y=y, z = Dissolved_Oxygen),
                      colour = "grey10",
                      breaks = MakeBreaks(binwidth = 1),
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("do"),
                          guide = guide_legend(reverse=T),
                          limits = as.character(seq(1, 17, 1))) +
        # geom_text_contour(data = interp[[2]],
        #                   aes(x=x, y=y, z = Dissolved_Oxygen),
        #                   #skip = 1,
        #                   check_overlap = TRUE,
        #                   size = 6,
        #                   #stroke = 0.2,
        #                   breaks = MakeBreaks(binwidth = 1)) + #,nudge_y = 0.5
        geom_rect(data = rectdf, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = bottom,
                     aes(x=x, y=y), fill = "grey90", colour = "grey20") +
        geom_point(data = samp_locs,
                   aes(x = x, y = - y),
                   colour = "black",
                   size = 0.5) +
        geom_point(data = C_oxy_locs,
                   aes(x = x, y = y),
                   size = 6,
                   colour = "black",
                   bg = C_oxy_locs$c,
                   shape = 24) +
        annotate("text",
                 label = "Dissolved Oxygen (mg/L)",
                 x = 3.9,
                 y = -5.7,
                 size = 9,
                 fontface = 2,
                 colour = "black") +
        labs(y = "Depth (m)") +
        annotation_custom(grob = oxy_grob, xmin = 9.2, xmax = 11.2, ymin = -6.4, ymax = -4.1) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border=element_blank(),
              axis.line = element_line(colour = "black"),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.ticks.length.y.left = (unit(2, "mm")),
              axis.ticks.length.y.right = (unit(2, "mm")),
              #axis.ticks.x = element_blank(),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust=0.5, vjust=0.5, face='bold'),
              plot.subtitle = element_text(hjust=0.5, vjust=0.5),
              legend.background = element_rect(fill = "transparent"),
              legend.direction = "horizontal",
              legend.position = c(0.43, 0.15),
              legend.key.size =  unit(8, "mm"),
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              plot.margin=grid::unit(c(0,0,0,0), "mm")) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                                   label.position = "bottom"))

      chlorPlot <- ggplot()+
        geom_tile(data = interp[[4]],
                  aes(x=x, y=y, fill = factor(Chlorophyll)),
                  alpha = 0.5) +
        scale_x_continuous(limits = c(0.5, 15.95),
                           expand = c(0, 0),
                           breaks = seq(0, 14, by = 2)) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .05))) +
        stat_contour2(data = interp[[4]],
                      aes(x=x, y=y, z = Chlorophyll),
                      colour = "grey10",
                      breaks = chl_brk,
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("chl"),
                          guide = guide_legend(reverse=T),
                          labels = c("20", "40", "60", "80", "120", "160",
                                     "200", "300", "400", "> 400"),
                          limits = c(as.character(seq(20, 80, 20)),
                                     "120", "160", "200", "300", "400", "1000")) +
        # geom_text_contour(data = interp[[4]],
        #                   aes(x=x, y=y, z = Chlorophyll),
        #                   #skip = 2,
        #                   size = 6,
        #                   check_overlap = TRUE,
        #                   #stroke = 0.2,
        #                   breaks = as.numeric(chl_brk)) +
        geom_rect(data = rectdf, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = bottom,
                     aes(x=x, y=y), fill = "grey90", colour = "grey20") +
        geom_point(data = samp_locs,
                   aes(x = x, y = - y),
                   colour = "black",
                   size = 0.5) +
        annotate("text",
                 label = expression('bold(paste("F-Chlorophyll (", mu,"g/L)"))'),
                 x = 3.7,
                 y = -5.8,
                 size = 9,
                 colour = "black", parse = TRUE) +
        labs(x = "Distance From Entrance (km)",
             y = "") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              #panel.border = element_blank(),
              #panel.border = element_rect(fill = NA, colour = "#CCCCCC"),
              axis.line = element_line(colour = "black"),
              axis.line.x = element_blank(),
              axis.line.y = element_blank(),
              axis.ticks.length.y.left = (unit(2, "mm")),
              axis.ticks.length.y.right = (unit(2, "mm")),
              #axis.ticks.x = element_blank(),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              axis.text.x = element_blank(),
              axis.title.x = element_blank(),
              plot.title = element_text(hjust=0.5, vjust=0.5, face='bold'),
              plot.subtitle = element_text(hjust=0.5, vjust=0.5),
              legend.background = element_rect(fill = "transparent"),
              legend.direction = "horizontal",
              legend.position = c(0.43, 0.15),
              legend.key.size =  unit(8, "mm"),
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              plot.margin=grid::unit(c(0,0,0,0), "mm")) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                                   label.position = "bottom"))

      tempPlot <- ggplot()+
        geom_tile(data = interp[[3]],
                  aes(x=x, y=y, fill = factor(Temperature))) +
        scale_x_continuous(limits = c(0.5, 15.95),
                           expand = c(0, 0),
                           breaks = seq(0, 14, by = 2)) +
        scale_y_continuous(expand = expand_scale(mult = c(0, .05)))+
        stat_contour2(data = interp[[3]],
                      aes(x=x, y=y, z = Temperature),
                      colour = "grey10",
                      breaks = MakeBreaks(binwidth = 1),
                      size = 0.1) +
        scale_fill_manual(values = surfer_cols("temp"),
                          guide = guide_legend(reverse=T),
                          limits = as.character(seq(11, 33, 1))) +
        # geom_text_contour(data = interp[[3]],
        #                   aes(x=x, y=y, z = Temperature),
        #                   size = 6,
        #                   #stroke = 0.2,
        #                   breaks = MakeBreaks(binwidth = 1)) + #,nudge_y = 0.5
        geom_rect(data = rectdf, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                  fill = "black") +
        geom_polygon(data = bottom,
                     aes(x=x, y=y), fill = "grey90", colour = "grey20") +
        geom_point(data = samp_locs,
                   aes(x = x, y = - y),
                   colour = "black",
                   size = 0.5) +
        annotate("text",
                 label = expression('bold(paste("Temperature (", degree,"C)"))'),
                 x = 3.4,
                 y = -5.7,
                 size = 9,
                 fontface = 2,
                 colour = "black", parse = TRUE) +
        labs(x = "Distance From Canning Bridge (km)",
             y = "") +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank(),
              panel.border=element_blank(),
              axis.line = element_line(colour = "black"),
              axis.line.y = element_blank(),
              axis.ticks.length.y.left = (unit(2, "mm")),
              axis.ticks.length.y.right = (unit(2, "mm")),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              axis.text.x = element_text(vjust = 0.5),
              plot.title = element_text(hjust=0.5, vjust=0.5, face='bold'),
              plot.subtitle = element_text(hjust=0.5, vjust=0.5),
              legend.background = element_rect(fill = "transparent"),
              legend.direction = "horizontal",
              legend.position = c(0.43, 0.15),
              legend.key.size =  unit(8, "mm"),
              legend.title = element_blank(),
              legend.text = element_text(size = 12),
              plot.margin=grid::unit(c(0,0,0,0), "mm")) +
        guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                                   label.position = "bottom"))

      # full length plots
      #create list of plotGrobs
      plta <- lapply(list(salPlot, doPlot, chlorPlot, tempPlot), ggplotGrob)
      #rbind (i.e. 1 column) size arg matters!
      surfers <- rbind(plta[[1]], plta[[2]], plta[[3]], plta[[4]], size = "first")
      pdf_name <- paste0(path, "/plots/", "canning_", ymd(samp_date), "_surfer.pdf")
      cat(paste0(pdf_name,"\n"))
      #add margin padding coarse but effective
      surfers_pad <- gtable::gtable_add_padding(surfers, padding = unit(c(1,4,3,4), "cm"))

      ggsave(plot = grid.draw(surfers_pad), filename = pdf_name, width=28, height=18)

    } else {
      stop(paste0("Function expecting only 2 excel workbooks for one monitoring period, ",
                  "or perhaps you need the alternate surfR function."))
    }
  })

}
