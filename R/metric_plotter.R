# Plot any metric as a single surfer style plot

#' Reads in external sonde data, combines with internal data sets and produces a
#' single surfer plot of a user defined metric.
#' 
#' \code{plot_metric} takes a file path to monitoring data and creates a single 
#'      surfer plot of a user defined metric. Plots are saved to the current 
#'      working directory. 
#'      
#' @details The metric plot shows a cross-section of the river where the metric 
#'      of interest has been interpolated between sonde locations. A thin plate 
#'      spline has been used for the interpolation.
#'      
#'      The function expects 1 excel workbook for the one monitoring run. The user 
#'      is expected to have appended raw data from the 2 sondes into one data file,
#'      ensuring that data formats are consistent for all columns. The edited sheet 
#'      must be renamed to "edited data". Also the 'dep m' column should not be 
#'      included.
#'      
#'      Upon reading in the data, the function will present the user with a choice 
#'      of metric to plot via numerical keyboard entry.
#'     
#'      A river "bottom" is displayed which puts the interpolation in context.
#'      The river bottom has been derived from a combination of historical
#'      maximum depths at sampling locations as well as station points extracted
#'      from a "best" navigation line over the latest corporate bathymetry. See
#'      project documentation for further details.
#'      
#'      Note this plot is designed for ad-hoc visualisations.
#'     
#' @param path Character string file path to location of edited xlsx workbook. 
#' User is expected to have appended data from 2 sondes into one work sheet, 
#' standardising the data and naming the sheet "edited data". Data variables should 
#' only consist of the date, site and vpos columns along with measured metrics of 
#' interest. Do not include GPS coordinates, time  or the 'dep m' columns. 
#'     
#' @param samp Colour represented as a character string (e.g. "red", "cyan") for 
#' the sampling locations in the water column. Changing colour can help them stand 
#' out in the different colour scales used for the interpolation. Default "red".
#' 
#' @param colscale A single letter designating a Viridis colour scale to be used 
#' for the interpolation. Any letter from A-H. Default "d".
#' 
#' @return A png format single panel surfer plot of the Canning River for the user's
#' choice of metric saved to the working directory. Plot name will contain date 
#' and chosen metric.
#'
#' @examples
#' \dontrun{
#' plot_metric(path = "Z:/DEC/MonitoringProgram/Data", samp = "red", 
#' colscale = "a")
#' 
#' }
#'
#' @author Bart Huntley, \email{bart.huntley@@dbca.wa.gov.au}
#'
#' For more details see  \url{https://dbca-wa.github.io/rivRmon/index.html}
#' {the rivRmon website}
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @import grid
#' @import fields
#' @import metR
#' @importFrom janitor clean_names
#' @importFrom raster raster interpolate reclassify rasterToPoints merge
#' @importFrom stats complete.cases
#' @importFrom lubridate ymd
#' @importFrom sp coordinates
#' 
#' @export
plot_metric <- function(path, samp = "red", colscale = "d"){
  
  # make folder for output
  folder <- file.path("metric_plots")
  if (!file.exists(folder)) {
    dir.create(folder)
  }
  
  # ensure parameters are in correct case for usage
  colscale <- toupper(colscale)
  
  # read in data
  samp_data <- sonde_reader_metric(path = path)|>
    dplyr::filter(site != "FP2.1")
  
  river <- ifelse(sum(stringr::str_detect(unique(samp_data[['site']]), "BLA")) == 1, "s", "c")
  
  samp_date <- samp_data |>
    dplyr::pull(date)|>
    unique()
  
  samp_date <- gsub("-", "", samp_date)
  
  
  # user chooses metric from available data
  opts <- samp_data |> 
    dplyr::select(-date, -'dep m', -site) |>
    names()
  cli::cli_ol(opts)
  num <- readline(prompt = "Choose a variable: ")
  metric <- opts[as.numeric(num)]
  
  if(river == "c"){
    # join sites to WQ data
    samp_data <- samp_data |>
      dplyr::mutate(site = ifelse(site == "NICIN", "NIC-IN", site )) # spelling mismatch
    
    comb_data <- dplyr::left_join(samp_data, C_sitesdf, by = "site")
    d_reduced <- comb_data|>
      dplyr::select(site, !! sym(metric), 'dep m',
                    dist_bridg, max_depth)|>
      janitor::clean_names()
    
    #### bottom adjust
    daily_depth <- d_reduced|>
      dplyr::group_by(dist_bridg)|>
      dplyr::summarise(d_depth = -1*(max(dep_m) + 0.2))|>
      dplyr::mutate(dist_bridg = dist_bridg/1000)
    
    C_bottom_open1 <- C_bottom_open|>
      dplyr::left_join(daily_depth, by = c("x" = "dist_bridg"))|>
      dplyr::mutate(y = case_when(
        !is.na(d_depth) ~ d_depth,
        TRUE ~ y
      ))|>
      dplyr::select(-d_depth)
    
    C_bottom_weir1 <- C_bottom_weir|>
      dplyr::left_join(daily_depth, by = c("x" = "dist_bridg"))|>
      dplyr::mutate(y = case_when(
        !is.na(d_depth) ~ d_depth,
        TRUE ~ y
      ))|>
      dplyr::select(-d_depth)
    
    # Set up param to plot
    sparams <- names(d_reduced)[2]
    
    # filter sampling data for separate interpolations
    d_all <- d_reduced[stats::complete.cases(d_reduced),]|>
      dplyr::mutate(y = -1 * dep_m, x = dist_bridg/1000)
    
    d_low <- d_reduced[stats::complete.cases(d_reduced),]|>
      dplyr::mutate(y = -1 * dep_m, x = dist_bridg/1000)|>
      dplyr::filter(x < 11.3)
    
    d_up <- d_reduced[stats::complete.cases(d_reduced),]|>
      dplyr::mutate(y = -1 * dep_m, x = dist_bridg/1000)|>
      dplyr::filter(x > 11.3)
    
    # Based on TPS interps from here
    tps_list_all <- vector("list", length(sparams))
    tps_list_weir <- vector("list", length(sparams))
    names(tps_list_all) <- sparams
    names(tps_list_weir) <- sparams #will combine upper and lower here
    
    ## for all river no weir open
    d1 <- d_all[,c("x", "y", sparams)]
    names(d1)[3] <- "value"
    sp::coordinates(d1) <- ~x + y
    
    tpsmodall <- fields::Tps(sp::coordinates(d1), d1$value)
    C_grd_all_R <- raster::raster(C_grd_all)
    tps_surfaceall <- raster::interpolate(C_grd_all_R, tpsmodall)
    tps_surfaceall[tps_surfaceall < 0] <- 0.1 # spline unfortunately interps to neg vals
    
    
    tps_points <- raster::rasterToPoints(tps_surfaceall, spatial = TRUE)
    tps_df <- data.frame(tps_points)[-4]  # ditch option
    names(tps_df)[1] <- sparams
    tps_list_all[[1]] <- tps_df
    
    
    ## for river when weir is open
    d1 <- d_low[,c("x", "y", sparams)]
    d2 <- d_up[,c("x", "y", sparams)]
    names(d1)[3] <- "value"
    names(d2)[3] <- "value"
    sp::coordinates(d1) <- ~x + y
    sp::coordinates(d2) <- ~x + y
    
    
    # for lower
    tpsmodlc <- fields::Tps(sp::coordinates(d1), d1$value)
    C_grd_low_R <- raster::raster(C_grd_low)
    tps_surfacelc <- raster::interpolate(C_grd_low_R, tpsmodlc)
    tps_surfacelc[tps_surfacelc < 0] <- 0.1  ####WEIRD NEG SYMBOL REMOVED
    
    # for upper
    tpsmoduc <- fields::Tps(sp::coordinates(d2), d2$value)
    C_grd_up_R <- raster::raster(C_grd_up)
    tps_surfaceuc <- raster::interpolate(C_grd_up_R, tpsmoduc)
    tps_surfaceuc[tps_surfaceuc < 0] <- 0.1 ####WEIRD NEG SYMBOL REMOVED
    
    # make one raster
    comb_tps_surface <- raster::merge(tps_surfacelc, tps_surfaceuc)
    
    
    tps_points <- raster::rasterToPoints(comb_tps_surface, spatial = TRUE)
    tps_df <- data.frame(tps_points)[-4]  # ditch option
    names(tps_df)[1] <- sparams[1]
    tps_list_weir[[1]] <- tps_df
    
    # make sample collection points
    samp_locs <- comb_data|>
      janitor::clean_names()|>
      dplyr::mutate(dist_bridg = dist_bridg/1000)|>
      dplyr::rename(x = dist_bridg, y = dep_m)|>
      dplyr::select(site, x, y)
    
    samp_labels <- c("SCB2", "SAL", "SHELL","RIV", "CASMID", "KEN", "BAC", 
                     "KS7", "NIC", "ELL", "BACD500", "PO2", "KS9", "PAC")
    samp_labels_locs <- C_sitesdf|>
      dplyr::filter(site %in% samp_labels)
    
    # Logic for weir no weir
    cannoxy <- c("KENU300", "BACD500", "BACD300", "BACU300", "PO2", "GRE", 
                 "MASD50", "NICD200", "KS9", "PAC", "MACD50")
    if(sum(cannoxy %in% samp_locs$site) > 0 ){
      bottom <- C_bottom_weir1
      interp <- tps_list_weir
      C_blockdf <- C_blockdf_weir
    } else {
      bottom <- C_bottom_open1
      interp <- tps_list_all
      C_blockdf <- C_blockdf_all
    }
    
    # construct pretty date
    sday <- just_nums(as.numeric(substr(samp_date, 7, 8)))
    sdate <- paste(sday, format(ymd(samp_date), "%b %Y"), sep = " ")
    
    # data frame of sites in this run
    sites_this_week <- tibble(site = unique(d_all$site))
    
    # restrict label data to sites sampled
    samp_labels_locs <- samp_labels_locs|>
      dplyr::right_join(sites_this_week, by = "site")
    
    # black out areas for plotting
    rectdf <- C_blockdf|>
      anti_join(sites_this_week, by = "site")
    
    p <- ggplot()+
      geom_tile(data = interp[[1]],
                aes(x=x, y=y, fill = pull(interp[[1]][1])),
                show.legend = c(fill = TRUE)) +
      scale_x_continuous(limits = c(0.5, 15.95),
                         expand = c(0, 0),
                         breaks = seq(0, 14, by = 2)) +
      scale_y_continuous(expand = expansion(mult = c(0, .05))) +
      stat_contour2(data = interp[[1]],
                    aes(x=x, y=y, z = pull(interp[[1]][1])),
                    colour = "grey10",
                    breaks = MakeBreaks(),
                    linewidth = 0.1) +
      scale_fill_viridis_c(option = colscale,
                           alpha = 0.7,
                           name = metric) +
      geom_rect(data = rectdf, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                fill = "black") +
      geom_polygon(data = bottom,
                   aes(x=x, y=y), fill = "grey90", colour = "grey20") +
      geom_text(data = samp_labels_locs,
                aes(x = dist_bridg/1000, y = 0.7, label = site, fontface = 2),
                size = 1.8,
                colour = "black",
                alpha = 1,
                check_overlap = TRUE) +
      geom_point(data = samp_locs,
                 aes(x = x, y = - y),
                 colour = samp,
                 size = 0.1) +
      labs(title = paste0(sdate, " plot of ", metric),
           y = "Depth (m)",
           x = "Distance From Canning Bridge (km)") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border=element_blank(),
            axis.line = element_line(colour = "black"),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.length.y.left = (unit(2, "mm")),
            axis.ticks.length.y.right = (unit(2, "mm")),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            plot.title = element_text(hjust=0.5, vjust=0.5, face='bold'),
            legend.text = element_text(size = 12),
            plot.margin=grid::unit(c(0,0,0,0), "mm"))
    
    metricpng <- gsub("/", "_", metric)
    metricpng <- gsub("-", "_", metricpng)
    metricpng <- gsub("%", "perc", metricpng)
    pname <- paste0("metric_plots/", "Canning_", samp_date, "_", metricpng, ".png")
    
    ggsave(filename = pname, p, width = 260, height = 100, units = "mm")
  } else {
    
    # swan data plots
    # join sites to WQ data
    comb_data <- dplyr::left_join(samp_data, S_sitesdf, by = "site")
    d_reduced <- comb_data|>
      dplyr::select(site, !! sym(metric), 'dep m',
                    dist_mouth, max_depth)|>
      janitor::clean_names()
    
    #### bottom adjust
    daily_depth <- d_reduced|>
      dplyr::group_by(dist_mouth)|>
      dplyr::summarise(d_depth = -1*(max(dep_m) + 0.2))|>
      dplyr::mutate(dist_mouth = dist_mouth/1000)
    
    S_bottom1 <- S_bottom|>
      dplyr::left_join(daily_depth, by = c("x" = "dist_mouth"))|>
      dplyr::mutate(y = case_when(
        !is.na(d_depth) ~ d_depth,
        TRUE ~ y
      ))|>
      dplyr::select(-d_depth)
    
    S_bottom_nar1 <- S_bottom_nar|>
      dplyr::left_join(daily_depth, by = c("x" = "dist_mouth"))|>
      dplyr::mutate(y = case_when(
        !is.na(d_depth) ~ d_depth,
        TRUE ~ y
      ))|>
      dplyr::select(-d_depth)
    
    # Set up param to plot
    sparams <- names(d_reduced)[2]
    
    # create interpolations and store in  separate lists
    d_all <- d_reduced[stats::complete.cases(d_reduced),]|>
      dplyr::mutate(y = -1 * dep_m, x = dist_mouth/1000)
    
    d_nar <- d_reduced[stats::complete.cases(d_reduced),]|>
      dplyr::mutate(y = -1 * dep_m, x = dist_mouth/1000)|>
      dplyr::filter(x >= 21)
    
    ## based on TPS interps from here
    tps_list_all <- vector("list", length(sparams))
    names(tps_list_all) <- sparams
    
    tps_list_n_s <- vector("list", length(sparams))
    names(tps_list_n_s) <- sparams
    
    d1all <- d_all[,c("x", "y", sparams)]
    names(d1all)[3] <- "value"
    sp::coordinates(d1all) <- ~x + y
    
    tpsmodall <- fields::Tps(sp::coordinates(d1all), d1all$value)
    S_grd_all_R <- raster::raster(S_grd_all)
    tps_surfaceall <- raster::interpolate(S_grd_all_R, tpsmodall)
    tps_surfaceall[tps_surfaceall < 0] <- 0.1 # spline unfortunately interps to neg vals
    
    
    tps_spall <- raster::rasterToPoints(tps_surfaceall, spatial = TRUE)
    tps_dfall <- data.frame(tps_spall)[-4]# ditch option
    names(tps_dfall)[1] <- sparams
    tps_list_all[[1]] <- tps_dfall
    
    
    
    # for narrows up TPS ## changed
    # make a mask to crop the whole river to above Narrows
    msk <- raster::raster(M_grd_nar)
    
    d1all <- d_all[,c("x", "y", sparams)]
    names(d1all)[3] <- "value"
    sp::coordinates(d1all) <- ~x + y
    
    tpsmodnar <- fields::Tps(coordinates(d1all), d1all$value)
    S_grd_nar_R <- raster::raster(S_grd_nar) 
    tps_surfacenar <- raster::interpolate(S_grd_nar_R, tpsmodnar)
    tps_surfacenar_m <- raster::crop(tps_surfacenar, msk) # crop all of river
    tps_surfacenar_m[tps_surfacenar_m < 0] <- 0.1 # spline unfortunately interps to neg vals
    
    
    tps_spnar <- raster::rasterToPoints(tps_surfacenar_m, spatial = TRUE)
    tps_dfnar <- data.frame(tps_spnar)[-4]# ditch option
    names(tps_dfnar)[1] <- sparams
    tps_list_n_s[[1]] <- tps_dfnar
    
    
    # make sample collection points
    samp_locs <- comb_data|>
      dplyr::mutate(dist_mouth = dist_mouth/1000)|>
      dplyr::rename(x = dist_mouth, y = 'dep m')|>
      dplyr::select(site, x, y)
    
    site_labs <- S_sitesdf|>
      dplyr::filter(site != "SRP_RSSA") |>
      dplyr::filter(site != "SANDBR" & site != "KMO" & site != "VIT" & 
                      site != "BREW" & site != "SWL148" & site != "MATCENT" &
                      site != "HURL" & site != "MYC" & site != "BWR1" &
                      site != "BWR2" & site != "BWR4" & site != "BWR5")
    
    site_labs_upper <- S_sitesdf|>
      dplyr::filter(site != "SRP_RSSA") |>
      dplyr::filter(site != "VIT" & site != "HURL" & site != "MYC" & 
                      site != "BWR1" & site != "BWR2" & site != "BWR4" & 
                      site != "BWR5")
    
    
    # construct pretty date
    sday <- just_nums(as.numeric(substr(samp_date, 7, 8)))
    sdate <- paste(sday, format(ymd(samp_date), "%b %Y"), sep = " ")
    
    # data frame of sites in this run
    sites_this_week <- tibble(site = unique(d_all$site))
    
    ## All River
    # black out areas for plotting
    rectdf_all <- S_blockdf_all |>
      dplyr::filter(site != "BREW" & site != "SWL148" & site != "MATCENT" &
                      site != "HURL" & site != "MYC" & site != "BWR1" &
                      site != "BWR2" & site != "BWR4" & site != "BWR5") |>
      anti_join(sites_this_week, by = "site")
    
    p <- ggplot()+
      geom_tile(data = tps_list_all[[1]],
                aes(x=x, y=y, fill = pull(tps_list_all[[1]][1])),
                show.legend = c(fill = TRUE)) +
      scale_x_continuous(limits = c(-1, 51.6),
                         expand = c(0, 0),
                         breaks = seq(0, 50, by = 5)) +
      scale_y_continuous(expand = expansion(mult = c(0, .05))) +
      stat_contour2(data = tps_list_all[[1]],
                    aes(x=x, y=y, z = pull(tps_list_all[[1]][1])),
                    colour = "grey10",
                    breaks = MakeBreaks(),
                    linewidth = 0.1) +
      scale_fill_viridis_c(option = colscale,
                           alpha = 0.7,
                           name = metric) +
      geom_rect(data = rectdf_all, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                fill = "black") +
      geom_polygon(data = S_bottom1,
                   aes(x=x, y=y), fill = "grey90", colour = "grey20") +
      geom_text(data = site_labs,
                aes(x = dist_mouth/1000, y = 0.7, label = site, fontface = 2),
                size = 2,
                colour = "black",
                alpha = 1,
                check_overlap = TRUE) +
      geom_point(data = samp_locs,
                 aes(x = x, y = - y),
                 colour = samp,
                 size = 0.1) +
      labs(title = paste0(sdate, " plot of ", metric),
           y = "Depth (m)",
           x = "Distance From Estuary Mouth (km)") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border=element_blank(),
            axis.line = element_line(colour = "black"),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.length.y.left = (unit(2, "mm")),
            axis.ticks.length.y.right = (unit(2, "mm")),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            plot.title = element_text(hjust=0.5, vjust=0.5, face='bold'),
            legend.text = element_text(size = 12),
            plot.margin=grid::unit(c(0,0,0,0), "mm"))
    
    metricpng <- gsub("/", "_", metric)
    metricpng <- gsub("-", "_", metricpng)
    metricpng <- gsub("%", "perc", metricpng)
    pname <- paste0("metric_plots/", "Swan_", samp_date, "_", metricpng, ".png")
    
    ggsave(filename = pname, p, width = 260, height = 100, units = "mm")
    
    # for upper Swan
    rectdf_nar <- S_blockdf_nar |>
      dplyr::filter(site != "BREW" & site != "SWL148" & site != "MATCENT" &
                      site != "HURL" & site != "MYC" & site != "BWR1" &
                      site != "BWR2" & site != "BWR4" & site != "BWR5") |>
      anti_join(sites_this_week, by = "site")
    
    pnar <- ggplot()+
      geom_tile(data = tps_list_n_s[[1]],
                aes(x=x, y=y, fill = pull(tps_list_n_s[[1]][1])),
                show.legend = c(fill = TRUE)) +
      scale_x_continuous(limits = c(20.95,51.6),
                         expand = c(0, 0),
                         breaks = c(25, 30, 35, 40, 45, 50)) +
      scale_y_continuous(breaks = c(-8, -6, -4, -2, 0),
                         expand = expansion(mult = c(0, .05)))+
      stat_contour2(data = tps_list_n_s[[1]],
                    aes(x=x, y=y, z = pull(tps_list_n_s[[1]][1])),
                    colour = "grey10",
                    breaks = MakeBreaks(),
                    linewidth = 0.1) +
      scale_fill_viridis_c(option = colscale,
                           alpha = 0.7,
                           name = metric) +
      geom_rect(data = rectdf_nar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax + 0.05),
                fill = "black") +
      geom_polygon(data = S_bottom_nar1,
                   aes(x=x, y=y), fill = "grey90", colour = "grey20") +
      geom_text(data = filter(site_labs_upper, dist_mouth/1000 >= 21),
                aes(x = dist_mouth/1000, y = 0.7, label = site, fontface = 2),
                size = 2,
                colour = "black",
                alpha = 1,
                check_overlap = TRUE) +
      geom_point(data = filter(samp_locs, x >= 21),
                 aes(x = x, y = - y),
                 colour = samp,
                 size = 0.1) +
      labs(title = paste0(sdate, " plot of ", metric),
           y = "Depth (m)",
           x = "Distance From Estuary Mouth (km)") +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            panel.border=element_blank(),
            axis.line = element_line(colour = "black"),
            axis.line.x = element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.length.y.left = (unit(2, "mm")),
            axis.ticks.length.y.right = (unit(2, "mm")),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            plot.title = element_text(hjust=0.5, vjust=0.5, face='bold'),
            legend.text = element_text(size = 12),
            plot.margin=grid::unit(c(0,0,0,0), "mm"))
    
    metricpng <- gsub("/", "_", metric)
    metricpng <- gsub("-", "_", metricpng)
    metricpng <- gsub("%", "perc", metricpng)
    pnarname <- paste0("metric_plots/","Swan_Upper_", samp_date, "_", metricpng, ".png")
    
    ggsave(filename = pnarname, pnar, width = 260, height = 100, units = "mm")
    
  }
  
}