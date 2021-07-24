
get_cols <- function(){
  tibble(
    col = RColorBrewer::brewer.pal(n = 4, name = 'Set2'),
    season = c('spring','summer','winter','fall'))
}

plot_time_season_accuracy <- function(fileout, preds_obs_fl){

  sesn_cols <- get_cols()
  plot_data <- read_csv(preds_obs_fl) %>%
    mutate(doy = lubridate::yday(Date),
           year = lubridate::year(Date),
           season = case_when(
             doy >= 79 & doy < 171 ~ 'spring',
             doy >= 171 & doy < 265 ~ 'summer',
             doy >= 265 & doy < 355 ~ 'fall',
             doy >= 355 | doy < 79 ~ 'winter')
           ) %>%
    group_by(season, year, site_id) %>%
    summarize(rmse = rmse(wtemp_EALSTM, wtemp_obs)) %>%
    group_by(season, year) %>% summarize(rmse = median(rmse)) %>%
    mutate(col = case_when(
      season == 'spring' ~ filter(sesn_cols, season == 'spring')$col,
      season == 'summer' ~ filter(sesn_cols, season == 'summer')$col,
      season == 'fall' ~ filter(sesn_cols, season == 'fall')$col,
      season == 'winter' ~ filter(sesn_cols, season == 'winter')$col
    ))

  png(file = fileout, width = 10, height = 6, units = 'in', res = 250)
  par(omi = c(0,0,0.05,0.05), mai = c(0.5,1,0,0), las = 1, mgp = c(2,.5,0), cex = 1.5)

  plot(NA, NA, xlim = c(1980, 2020), ylim = c(0.67, 1.42),
       ylab = "Median lake validation RMSE (°C)", xlab = "", axes = FALSE)

  axis(1, at = seq(1970, 2030, by = 5), tck = -0.01)
  axis(2, at = seq(0,10, by = 0.2), las = 1, tck = -0.01)

  for (var in c('spring', 'summer', 'fall', 'winter')){
    filter(plot_data, season == var) %>% (function(x){
      points(x$year, x$rmse, col = x$col, pch = 16, cex = 0.6)
      lines(x$year, x$rmse, col = x$col[1], lwd = 2)
      })
  }
  dev.off()
}


plot_data_coverage <- function(fileout, centroids_sf, preds_obs_fl){

  sesn_cols <- get_cols()
  plot_data <- read_csv(preds_obs_fl) %>%
    mutate(doy = lubridate::yday(Date),
           year = lubridate::year(Date),
           season = case_when(
             doy >= 79 & doy < 171 ~ 'spring',
             doy >= 171 & doy < 265 ~ 'summer',
             doy >= 265 & doy < 355 ~ 'fall',
             doy >= 355 | doy < 79 ~ 'winter')
    ) %>%
    group_by(season, year) %>% tally %>%
    mutate(col = case_when(
      season == 'spring' ~ filter(sesn_cols, season == 'spring')$col,
      season == 'summer' ~ filter(sesn_cols, season == 'summer')$col,
      season == 'fall' ~ filter(sesn_cols, season == 'fall')$col,
      season == 'winter' ~ filter(sesn_cols, season == 'winter')$col
    ))

  png(file = fileout, width = 10, height = 4, units = 'in', res = 250)
  par(omi = c(0,0,0.05,0.05), mai = c(0.5,1,0,0), las = 1, mgp = c(2,.5,0), cex = 1.5)

  plot(NA, NA, xlim = c(1980, 2020), ylim = c(0, 17000),
       ylab = "Suface temperature observations (#)", xlab = "", axes = FALSE)

  axis(1, at = seq(1970, 2030, by = 5), tck = -0.01)
  axis(2, at = seq(-5000 ,20000, by = 5000), labels = paste(seq(-5, 20, by = 5), 'k', sep = ''), las = 1, tck = -0.01)

  for (year in 1980:2020){
    winter_ht <- filter(plot_data, year == !!year, season == 'winter') %>% pull(n)
    spring_ht <- filter(plot_data, year == !!year, season == 'spring') %>% pull(n)
    summer_ht <- filter(plot_data, year == !!year, season == 'summer') %>% pull(n)
    fall_ht <- filter(plot_data, year == !!year, season == 'fall') %>% pull(n)

    rect(year-0.5, ybottom = 0, xright = year+0.5, ytop = winter_ht,
         col = filter(sesn_cols, season == 'winter')$col, border = NA)
    rect(year-0.5, ybottom = winter_ht, xright = year+0.5, ytop = winter_ht + spring_ht,
         col = filter(sesn_cols, season == 'spring')$col, border = NA)
    rect(year-0.5, ybottom = winter_ht + spring_ht, xright = year+0.5, ytop = winter_ht + spring_ht + summer_ht,
         col = filter(sesn_cols, season == 'summer')$col, border = NA)
    rect(year-0.5, ybottom = winter_ht + spring_ht + summer_ht, xright = year+0.5, ytop = winter_ht + spring_ht + summer_ht + fall_ht,
         col = filter(sesn_cols, season == 'fall')$col, border = NA)
  }
  dev.off()
}

plot_spatial_accuracy <- function(metadata_fl, preds_obs_fl, cellsize, model_id){

  min_obs <- 100 # minimum number of observations per cell to plot a color
  plot_proj <- "+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=999999.9999898402 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

  pred_obs <- read_csv(preds_obs_fl, col_types = 'cDdddd')

  sites_sf <- read.csv(metadata_fl) %>%
    filter(num_obs > 0) %>%
    st_as_sf(coords = c("lake_lon_deg", "lake_lat_deg"), crs = 4326)

  site_grid <- st_make_grid(sites_sf, cellsize = cellsize, square = TRUE, offset = c(-125, 25)) %>%
    st_as_sf() %>%
    select(geometry=x) %>%
    mutate(cell_id = row_number()) %>%
    st_transform(plot_proj)

  bin_breaks <- c(0, seq(0.75, 5.75, by = 0.05), 20)
  n_cols <- length(bin_breaks) - 1

  col_tbl <- tibble(val = bin_breaks,
         col = c(viridis::inferno(n = n_cols),
                 # we want all values above a certain threshold to be the same color
                 rep(tail(viridis::inferno(n = n_cols), 1L),
                     times = length(bin_breaks) - n_cols)),
         bin = cut(val, breaks = bin_breaks, right = F)) %>%
    select(-val)

  cell_obs <- st_transform(sites_sf, plot_proj) %>%
    st_intersection(site_grid, .) %>%
    st_drop_geometry() %>% select(cell_id, site_id) %>%
    right_join(pred_obs, by = 'site_id') %>%
    group_by(cell_id) %>%
    summarize(rmse = sqrt(mean((!!rlang::sym(model_id) - wtemp_obs)^2, na.rm=TRUE)),
              n = sum(!is.na(wtemp_obs))) %>%
    filter(n >= min_obs) %>%
    mutate(bin = cut(rmse, breaks = bin_breaks, right = F)) %>%
    left_join(col_tbl, by = 'bin')

  usa_sf <- sf::st_transform(spData::us_states, crs = 4326) %>%
    st_as_sf() %>%
    st_transform(plot_proj) %>% st_geometry()

  styled_grid <- site_grid %>% left_join(cell_obs, by = 'cell_id')
  old_par <- par(mai = c(0,0,0,0), xpd = NA)
  plot(usa_sf, col = NA, border = NA, reset = FALSE, setParUsrBB = TRUE)
  plot(st_geometry(styled_grid), col = styled_grid$col, border = styled_grid$col, add = TRUE, lwd = 0.25)
  plot(usa_sf, col = NA, border = 'grey80', add = TRUE)
  plot_dims <- par('usr')
  add_map_legend(plot_dims, bin_breaks, n_cols, col_fun = viridis::inferno, col_fun_dir = 1L, title = 'Validation error (RMSE °C)')
  par(old_par)
}

#' plot number of observed lakes within each cell
plot_spatial_coverage <- function(fileout, metadata_fl, preds_obs_fl, cellsize){

  plot_proj <- "+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=999999.9999898402 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

  pred_obs <- read_csv(preds_obs_fl, col_types = 'cDdddd')

  sites_sf <- read.csv(metadata_fl) %>%
    filter(num_obs > 0) %>%
    st_as_sf(coords = c("lake_lon_deg", "lake_lat_deg"), crs = 4326)

  site_grid <- st_make_grid(sites_sf, cellsize = cellsize, square = TRUE, offset = c(-125, 25)) %>%
    st_as_sf() %>%
    select(geometry=x) %>%
    mutate(cell_id = row_number()) %>%
    st_transform(plot_proj)


  bin_breaks <- c(1, 2, 5, 10, 20, 50, 100, 1000000)
  n_cols <- length(bin_breaks) - 1

  col_tbl <- tibble(val = bin_breaks,
                    col = c(viridis::mako(n = n_cols, direction = -1L),
                            # we want all values above a certain threshold to be the same color
                            rep(tail(viridis::mako(n = n_cols, direction = -1L), 1L),
                                times = length(bin_breaks) - n_cols)),
                    bin = cut(val, breaks = bin_breaks, right = F)) %>%
    filter(!is.na(bin)) %>%
    select(-val)

  cell_obs <- st_transform(sites_sf, plot_proj) %>%
    st_intersection(site_grid, .) %>%
    st_drop_geometry() %>% select(cell_id, site_id) %>%
    right_join(pred_obs, by = 'site_id') %>%
    group_by(cell_id) %>%
    summarize(n_ob_sites = length(unique(site_id))) %>%
    mutate(bin = cut(n_ob_sites, breaks = bin_breaks, right = F)) %>%
    left_join(col_tbl, by = 'bin')

  usa_sf <- sf::st_transform(spData::us_states, crs = 4326) %>%
    st_as_sf() %>%
    st_transform(plot_proj) %>% st_geometry()

  styled_grid <- site_grid %>% left_join(cell_obs, by = 'cell_id')

  png(file = fileout, width = 10, height = 6, units = 'in', res = 250)
  par(omi = c(0,0,0.00,0.00), mai = c(0.1,0.1,0.1,0.1), xpd = NA)

  plot(usa_sf, col = NA, border = NA, reset = FALSE, setParUsrBB = TRUE)
  plot(st_geometry(styled_grid), col = styled_grid$col, border = styled_grid$col, add = TRUE, lwd = 0.25)
  plot(usa_sf, col = NA, border = 'grey80', add = TRUE)
  plot_dims <- par('usr')
  add_map_legend(plot_dims, bin_breaks, n_cols, col_fun = viridis::mako, col_fun_dir = -1L, title = 'Number of observed lakes (#)')

  dev.off()

}


add_map_legend <- function(plot_dims, bin_breaks, n_cols, col_fun, col_fun_dir, title){
  total_leg_prc <- 0.3
  bin_w_prc <- total_leg_prc / n_cols
  bin_h_prc <- 0.045
  bin_w <- (plot_dims[2] - plot_dims[1]) * bin_w_prc
  total_leg_w <- (plot_dims[2] - plot_dims[1]) * total_leg_prc
  bin_h <- (plot_dims[4] - plot_dims[3]) * bin_h_prc
  x0 <- plot_dims[1] + (plot_dims[2] - plot_dims[1]) * 0.01
  y0 <- plot_dims[3] + (plot_dims[4] - plot_dims[3]) * 0.013
  for (i in 1:n_cols){
    # create a square for each color
    this_x0 <- x0+bin_w*(i-1)
    rect(xleft = this_x0, xright = this_x0+bin_w,
         ybottom = y0, ytop = y0+bin_h,
         col = col_fun(n = n_cols, direction = col_fun_dir)[i],
         border = NA)
    if (all(bin_breaks %% 1 == 0)){
      # don't plot the last number because all others above the second to last are the same color:
      if (i == 1){
        text(x = this_x0 + bin_w/2, y = y0+bin_h, bin_breaks[i], pos = 3, offset = 0.25, cex = 1)
      } else if (i == n_cols){
        text(x = this_x0 + bin_w/2, y = y0+bin_h, paste0(bin_breaks[i],'+'), pos = 3, offset = 0.25, cex = 1)
      } else {
        text_str <- sprintf('%s-%s', bin_breaks[i], bin_breaks[i+1]-1)
        text(x = this_x0 + bin_w/2, y = y0+bin_h, text_str, pos = 3, offset = 0.3, cex = 0.9)
      }
    } else {
      if (bin_breaks[i] %% 1 == 0 && bin_breaks[i] != 0){
        text(x = this_x0, y = y0+bin_h, bin_breaks[i], pos = 3, offset = 0.25, cex = 1)
      }
    }
  }
  text(x = x0, y = y0+bin_h *2.3, title, pos = 4, offset = 0, cex = 1.2)

}
#' create an accuracy grid for a 1:1 scatter plot
#' use heat/intensity to indicate how many values are in that cell
plot_accuracy <- function(preds_obs_fl, cellsize, model_id){

  model_type <- c(wtemp_EALSTM = 'EA-LSTM', wtemp_ERA5 = 'ERA5', wtemp_LM = 'Bachmann LM')
  acc_grid <- st_sfc(st_polygon(list(rbind(c(0,0), c(40,0), c(40,40), c(0,0))))) %>%
    st_make_grid(cellsize = cellsize) %>%
    st_as_sf() %>%
    select(geometry=x) %>%
    mutate(cell_id = row_number())

  bin_breaks <- c(1:9 %o% 10^(0:2), 1:3 %o% 10^(3), 200000000)
  n_cols <- length(bin_breaks) - 1

  col_fun <- viridis::mako
  col_fun_dir <- -1L

  col_tbl <- tibble(val = bin_breaks,
                    col = c(col_fun(n = n_cols, direction = col_fun_dir),
                            # we want all values above a certain threshold to be the same color
                            rep(tail(col_fun(n = n_cols, direction = col_fun_dir), 1L),
                                times = length(bin_breaks) - n_cols)),
                    bin = cut(val, breaks = bin_breaks, right = F)) %>%
    filter(!is.na(bin)) %>%
    select(-val)


  # lat is y, lon is x. Lon comes first:
  acc_vals <- read_csv(preds_obs_fl, col_types = 'cDdddd') %>%
    filter(!is.na(!!rlang::sym(model_id))) %>%
    st_as_sf(coords = c("wtemp_obs", model_id)) %>%
    st_intersection(acc_grid, .) %>%
    st_drop_geometry() %>% select(cell_id) %>%
    group_by(cell_id) %>%
    summarize(val = length(cell_id)) %>%
    mutate(bin = cut(val, breaks = bin_breaks, right = F)) %>%
    left_join(col_tbl, by = 'bin')

  styled_grid <- acc_grid %>% left_join(acc_vals, by = 'cell_id')
  old_par <- par(mai = c(.34,0.34,0,0), las = 1, mgp = c(1.4,.4,0), xaxs = 'i', yaxs = 'i')
  plot(st_geometry(styled_grid), col = styled_grid$col, border = styled_grid$col, reset = FALSE,
       ylab = sprintf("%s-predicted surface temperature (°C)", model_type[[model_id]]),
       xlab = "Observed surface temperature (°C)", axes = FALSE,
       ylim = c(0, 38), xlim = c(0, 38))
  box()
  axis(1, at = seq(0, 40, by = 5), tck = -0.01)
  axis(2, at = seq(0, 40, by = 5), las = 1, tck = -0.01)
  abline(0,1, lty = 'dashed')
  plot_dims <- par('usr')
  y_leg_prc <- 0.02 # bottom edge of colors
  x_leg_prc <- 0.88 # right edge of colors

  bin_w_prc <- 0.03
  bin_h_prc <- 0.01
  bin_w <- (plot_dims[2] - plot_dims[1]) * bin_w_prc
  bin_h <- (plot_dims[4] - plot_dims[3]) * bin_h_prc
  x0 <- plot_dims[1] + (plot_dims[2] - plot_dims[1]) * x_leg_prc - (plot_dims[2] - plot_dims[1]) * bin_w_prc
  y0 <- plot_dims[3] + (plot_dims[4] - plot_dims[3]) * y_leg_prc + (plot_dims[4] - plot_dims[3])* bin_h_prc * n_cols
  for (i in 1:n_cols){

    this_y0 <- y0-bin_h*(i-1)
    rect(xleft = x0, xright = x0+bin_w,
         ybottom = this_y0, ytop = this_y0-bin_h,
         col = col_fun(n = n_cols, direction = col_fun_dir)[i],
         border = NA)
    if (bin_breaks[i] %in% c(1,10,100,1000)){
      text(x = x0+bin_w, y = this_y0-bin_h/2, bin_breaks[i], pos = 4, offset = 0.25, cex = 1)
    }

  }

  text(x0+bin_w/2, y0, 'Count', pos = 3, cex = 1.2, offset = 0.75)
  par(old_par)
}

plot_space_raw_panel <- function(fileout, metadata_fl, preds_obs_fl,
                                 accuracy_cellsize = 0.5,
                                 space_cellsize = 1,
                                 model_ids){
  png(file = fileout, width = 7.55, height = 9.1, units = 'in', res = 250)
  par(omi = c(0,0.05,0.05,0.05), mai = c(0,0,0,0), las = 1, xaxs = 'i', yaxs = 'i')
  layout(matrix(c(1,1,1, 2,2,3,3,3,4,4,5,5,5,6,6), nrow = 3, byrow = TRUE))

  for (j in 1:3){
    plot_spatial_accuracy(metadata_fl, preds_obs_fl,
                          cellsize = space_cellsize,
                          model_id = model_ids[j])
    plot_accuracy(preds_obs_fl,
                  cellsize = accuracy_cellsize,
                  model_id = model_ids[j])
  }

  dev.off()

}
