
get_cols <- function(){
  tibble(
    col = RColorBrewer::brewer.pal(n = 4, name = 'Set2'),
    season = c('spring','summer','winter','fall'))
}

get_proj <- function(){
  "+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=999999.9999898402 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
}

get_model_type <- function(model_id){
  c(wtemp_EALSTM = 'EA-LSTM', wtemp_ERA5 = 'ERA5', wtemp_LM = 'Bachmann LM')[[model_id]]
}

plot_year_season_bias <- function(fileout, preds_obs_fl, model_id){
  # plot yearly (median) RMSE
  # plot DoY median bias for 3 day chunks
  # plot temperature bias for 3°C chunks
  # obs count vs RMSE


  # Bachmann uses DOY 152 to 273 or 274 (depending on leap-year; https://doi.org/10.3390/geosciences9070296)
  # I verified that we don't have any LM preds outside of that range, so I'm ok filtering on it:
  # Bachmann also calls this period "Summer" even though it is a bit wider than the normal definition of summer
  sesn_cols <- get_cols()
  plot_data <- read_csv(preds_obs_fl) %>%
    mutate(doy = lubridate::yday(Date),
           year = lubridate::year(Date),
           season = case_when(
             !is.na(wtemp_LM) ~ 'summer',
             TRUE ~ 'other')
    ) %>%
    # was calculating lake/season/year-specific RMSE before, then taking median.
    #    group_by(season, year, site_id) %>%
    group_by(season, year) %>%
    summarize(rmse = sqrt(mean((!!rlang::sym(model_id) - wtemp_obs +
                                  ifelse(model_id == 'wtemp_ERA5', 3.47, 0))^2, na.rm=TRUE)))

  title_text <- sprintf('%s test error (RMSE °C)', get_model_type(model_id))
  if (model_id == 'wtemp_ERA5'){
    title_text[1L] <- paste0('*debiased ', title_text[1L])
  }

  png(file = fileout, width = 10, height = 6, units = 'in', res = 250)
  par(omi = c(0,0,0.05,0.05), mai = c(0.5,1,0,0), las = 1, mgp = c(2,.5,0), cex = 1.5)

  plot(NA, NA, xlim = c(1980, 2020), ylim = c(1.3, 3.1),
       ylab = title_text,
       xlab = "", axes = FALSE)
  filter(plot_data, season == 'summer') %>% {points(.$year, .$rmse, col = 'black', type = 'o', pch = 16, lwd = 2)}
  filter(plot_data, season == 'other') %>% {points(.$year, .$rmse, col = 'grey40', type = 'o', pch = 22, lty = 'dashed', bg = 'white', lwd = 2)}
  axis(1, at = seq(1970, 2030, by = 5), tck = -0.01)
  axis(2, at = seq(0,10, by = 0.5), las = 1, tck = -0.01)
  dev.off()
}


plot_year_bias <- function(fileout, preds_obs_fl, model_id){
  # Bachmann uses DOY 152 to 273 or 274 (depending on leap-year; https://doi.org/10.3390/geosciences9070296)
  # I verified that we don't have any LM preds outside of that range, so I'm ok filtering on it:
  # Bachmann also calls this period "Summer" even though it is a bit wider than the normal definition of summer
  plot_data <- read_csv(preds_obs_fl) %>%
    mutate(doy = lubridate::yday(Date),
           year = lubridate::year(Date)) %>%
    # was calculating lake/season/year-specific RMSE before, then taking median.
    #    group_by(season, year, site_id) %>%
    group_by(year) %>%
    summarize(bias = mean(!!rlang::sym(model_id) - wtemp_obs, na.rm=TRUE))

  title_text <- sprintf('%s bias (°C)', get_model_type(model_id))

  bias_col <- get_bias_colors()

  png(file = fileout, width = 6, height = 6, units = 'in', res = 250)
  par(omi = c(0,0,0.05,0.05), mai = c(0.5,1,0,0), las = 1, mgp = c(2,.5,0), cex = 1.5)

  plot(NA, NA, xlim = c(1980, 2020), ylim = c(-4.3, 1.1),
       ylab = title_text,
       xlab = "", axes = FALSE)
  for (year in 1980:2020){
    these_data <- filter(plot_data, year == !!year)

    bias <- these_data$bias
    col <- ifelse(bias < 0, bias_col$cold, bias_col$hot)
    rect(xleft = year-0.5, xright = year+0.5, ybottom = 0, ytop = these_data$bias, col = scales::alpha(col, alpha = 0.5), border = col)
  }
  abline(h = 0)
  if (model_id == 'wtemp_ERA5'){
    abline(h = -3.47, lty = 'dashed')
  }
  axis(1, at = seq(1970, 2030, by = 10), tck = -0.01)
  axis(2, at = seq(-10,10, by = 1), las = 1, tck = -0.01)
  dev.off()
}

get_bias_colors <- function(){
  tibble('hot' = '#fc8d62', cold = "#00204DFF")
}
plot_doy_bias <- function(fileout, preds_obs_fl, model_id){
  # plot yearly (median) RMSE
  # plot DoY median bias for 3 day chunks
  # plot temperature bias for 3°C chunks
  # obs count vs RMSE


  # Bachmann uses DOY 152 to 273 or 274 (depending on leap-year; https://doi.org/10.3390/geosciences9070296)
  # I verified that we don't have any LM preds outside of that range, so I'm ok filtering on it:
  # Bachmann also calls this period "Summer" even though it is a bit wider than the normal definition of summer

  low_bin <- 0
  high_bin <- 366
  bin_w <- 3
  # make sure the last bin is inclusive of leap year
  bin_breaks <- c(seq(low_bin, high_bin - bin_w, by = bin_w), high_bin + 10)
  plot_data <- read_csv(preds_obs_fl) %>%
    mutate(doy = lubridate::yday(Date),
           upper_doy_bin = cut(doy, breaks = bin_breaks, labels = FALSE, right = FALSE) * bin_w) %>%
    # PROBABLY want to combine DOY 366 with 365 samples
    group_by(upper_doy_bin) %>%
    summarize(bias = mean(!!rlang::sym(model_id) - wtemp_obs, na.rm = TRUE))


  title_text <- sprintf('%s prediction bias(°C)', get_model_type(model_id))
  # if (model_id == 'wtemp_ERA5'){
  #   title_text[1L] <- paste0('*debiased ', title_text[1L])
  # }

  png(file = fileout, width = 6, height = 6, units = 'in', res = 250)
  par(omi = c(0.05,0.05,0.05,0.05), mai = c(0,0,0,0), las = 1, mgp = c(2,.5,0), cex = 1.5)

  zer_bias_r <- 6

  plot(NA, NA, ylim = c(-zer_bias_r - 2, zer_bias_r + 2), xlim = c(-zer_bias_r - 2, zer_bias_r + 2),
       ylab = title_text,
       xlab = "", axes = FALSE)

  bias_col <- get_bias_colors()


  # need to plot these as DOY bins!
  for (bin in unique(plot_data$upper_doy_bin)){
    these_data <- plot_data %>% filter(upper_doy_bin == bin)
    bias <- these_data$bias
    col <- ifelse(bias < 0, bias_col$cold, bias_col$hot)
    upper_alpha <- 2*pi * bin / high_bin
    lower_alpha <- 2*pi * (bin-bin_w) / high_bin
    upper_y0 <- cos(upper_alpha) * (bias+zer_bias_r)
    upper_y1 <- cos(upper_alpha) * zer_bias_r
    lower_y0 <- cos(lower_alpha) * (bias+zer_bias_r)
    lower_y1 <- cos(lower_alpha) * zer_bias_r
    upper_x0 <- sin(upper_alpha) * (bias+zer_bias_r)
    upper_x1 <- sin(upper_alpha) * zer_bias_r
    lower_x0 <- sin(lower_alpha) * (bias+zer_bias_r)
    lower_x1 <- sin(lower_alpha) * zer_bias_r

    # 0,0 is zer_bias_r bias

    if(!is.na(bias)){
      polygon(x = c(lower_x0, lower_x1, upper_x1, upper_x0, lower_x0), y = c(lower_y0, lower_y1, upper_y1, upper_y0, lower_y0),
              col = scales::alpha(col, alpha = 0.5), border = col)
    }

  }

  # the seasonal boundaries:
  seasons <- c('winter','spring','summer','fall')
  for (doy in c(79, 170.5, 262, 353.5)){
    alpha <- 2*pi * doy / 366
    x0 <- sin(alpha) * (zer_bias_r + 2)
    y0 <- cos(alpha) * (zer_bias_r + 2)
    lines(c(0, x0), c(0, y0))

    alpha <- 2*pi * (doy - 366/8) / 366
    x0 <- sin(alpha) * (zer_bias_r + 0.5)
    y0 <- cos(alpha) * (zer_bias_r + 0.5)

    rotate_angle <- 225 - doy / 366 * 360
    adj <- c(0.5, 0.5)
    if (abs(rotate_angle) > 115){
      rotate_angle = rotate_angle - 180
    } else {
      adj[2L] <- adj[2L] - 0.1
    }
    text(x0, y0, seasons[1L], srt = rotate_angle, adj = adj)
    seasons <- tail(seasons, -1L)
  }


  biases <- c("+1°C" = 0.8, "0°C" = -0.1, "-1°C" = -1.2, "-2°C" = -2.2, "-3°C" = -3.2)
  avoid_alphas <- tibble(values = c(-1:3), div = c(5,5,5,4,4), avoid_alpha = NA)
  doy <- 103
  for (i in 1:length(biases)){
    alpha <- 2*pi * doy / 366
    rotate_angle <-  90 - doy / 366 * 360
    x0 <- sin(alpha) * (zer_bias_r + biases[i])
    y0 <- cos(alpha) * (zer_bias_r + biases[i])
    text(x0, y0, names(biases)[i], srt = rotate_angle, adj = adj, cex = 0.65, font = 2)
    avoid_alphas$avoid_alpha[i] <- doy
    doy <- doy - 3 - i/3
  }


  cnt = 0
  for (doy in seq(0, 366, by = 1)){
    cnt = cnt+1

    upper_alpha <- 2*pi * doy / 366
    lower_alpha <-  2*pi * (doy + 1) / 366

    for (bias in avoid_alphas$values){
      this_alpha <- avoid_alphas %>% filter(values == bias)
      skip_range <- (round(this_alpha$avoid_alpha, 0) - 4):(round(this_alpha$avoid_alpha, 0) + 4)
      # whether to plot dash:
      if (cnt %% this_alpha$div != 0 & !(doy %in% skip_range)){
        lines((-bias + zer_bias_r) * c(sin(upper_alpha), sin(lower_alpha)), (-bias + zer_bias_r) * c(cos(upper_alpha), cos(lower_alpha)),
              col = 'grey20')
      }
    }

  }

  dev.off()
}

plot_tempbin_bias <- function(fileout, preds_obs_fl, model_id){
  # plot yearly (median) RMSE
  # plot DoY median bias for 3 day chunks
  # plot temperature bias for 3°C chunks
  # obs count vs RMSE


  # Bachmann uses DOY 152 to 273 or 274 (depending on leap-year; https://doi.org/10.3390/geosciences9070296)
  # I verified that we don't have any LM preds outside of that range, so I'm ok filtering on it:
  # Bachmann also calls this period "Summer" even though it is a bit wider than the normal definition of summer

  low_bin <- 0
  high_bin <- 36
  bin_w <- 2
  obs_min <- 100 # minimum number of obs in a bin for plotting
  bin_breaks <- seq(low_bin, high_bin, by = bin_w)
  plot_data <- read_csv(preds_obs_fl) %>%
    filter(wtemp_obs >= low_bin & wtemp_obs < high_bin) %>%
    mutate(upper_wtemp_bin = cut(wtemp_obs, breaks = bin_breaks, labels = FALSE, right = FALSE) * bin_w) %>%
    group_by(upper_wtemp_bin) %>%
    summarize(bias = mean(!!rlang::sym(model_id) - wtemp_obs, na.rm = TRUE),
              n_obs = sum(!is.na(!!rlang::sym(model_id)))) %>%
    filter(n_obs > obs_min)

  title_text <- sprintf('%s prediction bias (°C)', get_model_type(model_id))
  png(file = fileout, width = 4, height = 6, units = 'in', res = 250)
  par(omi = c(0,0,0.05,0.05), mai = c(1,1,0,0), las = 1, mgp = c(2,.5,0), cex = 1.5, xaxs = 'i', yaxs = 'i')

  bias_col <- get_bias_colors()
  plot(NA, NA, ylim = c(-7.2,5.8), xlim = c(0, 37),
       xlab = "Water temperature (°C)",
       ylab = title_text, axes = FALSE)
  # need to plot these as 2°C bins!
  for (bin in unique(plot_data$upper_wtemp_bin)){
    these_data <- plot_data %>% filter(upper_wtemp_bin == bin)
    bias <- these_data$bias
    col <- ifelse(bias < 0, bias_col$cold, bias_col$hot)
    rect(xleft =  bin - bin_w, xright = bin, ybottom = 0, ytop =bias, col = scales::alpha(col, alpha = 0.5), border = col)
  }
  abline(h = 0)
  if (model_id == 'wtemp_ERA5'){
    abline(h = -3.47, lty = 'dashed')
  }

  axis(1, at = seq(0,40, by = 10), las = 1, tck = -0.01)
  axis(2, at = seq(-10, 10, by = 2), tck = -0.01)
  dev.off()
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
    # was calculating lake/season/year-specific RMSE before, then taking median.
    #    group_by(season, year, site_id) %>%
    group_by(season, year) %>%
    summarize(rmse = rmse(wtemp_ERA5+3.47, wtemp_obs)) %>%
    #group_by(season, year) %>% summarize(rmse = median(rmse)) %>%
    mutate(col = case_when(
      season == 'spring' ~ filter(sesn_cols, season == 'spring')$col,
      season == 'summer' ~ filter(sesn_cols, season == 'summer')$col,
      season == 'fall' ~ filter(sesn_cols, season == 'fall')$col,
      season == 'winter' ~ filter(sesn_cols, season == 'winter')$col
    ))

  png(file = fileout, width = 10, height = 6, units = 'in', res = 250)
  par(omi = c(0,0,0.05,0.05), mai = c(0.5,1,0,0), las = 1, mgp = c(2,.5,0), cex = 1.5)

  plot(NA, NA, xlim = c(1980, 2020), ylim = c(1, 2.8),
       ylab = "EA-LSTM median lake test RMSE (°C)", xlab = "", axes = FALSE)

  axis(1, at = seq(1970, 2030, by = 5), tck = -0.01)
  axis(2, at = seq(0,10, by = 0.5), las = 1, tck = -0.01)

  y0 <- 1.55
  x0 <- 2013.5 # midpoint of legend line
  y_bmp <- 0.07
  x_wid <- 3.5

  for (var in c('spring', 'summer', 'fall', 'winter')){
    filter(plot_data, season == var) %>% (function(x){
      points(x$year, x$rmse, col = x$col, pch = 16, cex = 0.6)
      lines(x$year, x$rmse, col = x$col[1L], lwd = 2)
      # add legend here:
      lines(c(x0-x_wid/2, x0+x_wid/2), c(y0, y0), lwd = 2.5, col = x$col[1L])
      points(x0, y0, col = x$col, pch = 16, cex = 0.8)
      # then text:
      text(x0+x_wid/2, y0, stringr::str_to_sentence(var), pos = 4, offset = 0.55)
      # bump the y starting point up
      y0 <<- y0 - y_bmp
      })
  }
  dev.off()
}


plot_data_coverage <- function(centroids_sf, preds_obs_fl, panel_text){

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

  old_par <- par(mai = c(0.24, 0.44, 0, 0.1), las = 1, mgp = c(2, 0.4, 0))

  plot(NA, NA, xlim = c(1980, 2020), ylim = c(0, 17000),
       ylab = "Suface temperature observations (#)", xlab = "", axes = FALSE)

  axis(1, at = seq(1970, 2030, by = 5), tck = -0.01)
  axis(2, at = seq(-5000, 20000, by = 5000), labels = paste(seq(-5, 20, by = 5), 'k', sep = ''), las = 1, tck = -0.01)
  seasons <- c('winter','spring','summer','fall')

  for (year in 1980:2020){
    y0 <- 0
    for (season in seasons){
      this_ht <- filter(plot_data, year == !!year, season == !! season) %>% pull(n)
      rect(year-0.5, ybottom = y0, xright = year+0.5, ytop = this_ht + y0,
           col = filter(sesn_cols, season == !! season)$col, border = NA)
      y0 <- y0 + this_ht
    }
  }

  plot_dims <- par('usr')
  bin_w_prc <- 0.032
  bin_h_prc <- 0.06
  bin_gap_prc <- 0.015
  bin_w <- (plot_dims[2] - plot_dims[1]) * bin_w_prc
  bin_h <- (plot_dims[4] - plot_dims[3]) * bin_h_prc
  bin_gap <- (plot_dims[4] - plot_dims[3]) * 0.005

  x0 <- plot_dims[1] + (plot_dims[2] - plot_dims[1]) * 0.05
  y0 <- plot_dims[3] + (plot_dims[4] - plot_dims[3]) * 0.3

  for (j in 1:length(seasons)){
    this_y0 <- y0 + (j-1) * (bin_h + bin_gap)
    rect(xleft = x0, xright = x0 + bin_w, ybottom = this_y0, ytop = this_y0 + bin_h,
         col = filter(sesn_cols, season == seasons[j])$col, border = NA)
    text(x = x0 + bin_w, y = this_y0 + bin_h * 0.4 , stringr::str_to_title(seasons[j]), pos = 4, offset = 0.5)
  }
  y_panel <- plot_dims[4] - (plot_dims[4] - plot_dims[3]) * 0.033
  x_panel <- plot_dims[1] + (plot_dims[2] - plot_dims[1]) * 0.03
  text(x = x_panel, y = y_panel, adj = c(0.5, 0.5), panel_text, cex = 1.3)
  par(old_par)
}

plot_spatial_accuracy <- function(metadata_fl, preds_obs_fl, cellsize, model_id, panel_text){

  min_obs <- 100 # minimum number of observations per cell to plot a color
  plot_proj <- get_proj()

  pred_obs <- read_csv(preds_obs_fl, col_types = 'cDdddd')

  sites_sf <- read.csv(metadata_fl) %>%
    filter(num_obs > 0) %>%
    st_as_sf(coords = c("lake_lon_deg", "lake_lat_deg"), crs = 4326)

  site_grid <- st_make_grid(sites_sf, cellsize = cellsize, square = TRUE, offset = c(-125, 25)) %>%
    st_as_sf() %>%
    select(geometry=x) %>%
    mutate(cell_id = row_number()) %>%
    st_transform(plot_proj)

  bin_breaks <- c(0, seq(0.75, 5, by = 0.05), 20)
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
    # debiasing!!
    summarize(rmse = sqrt(mean((!!rlang::sym(model_id) - wtemp_obs +
                                  ifelse(model_id == 'wtemp_ERA5', 3.47, 0))^2, na.rm=TRUE)),
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
  y_panel <- plot_dims[4] - (plot_dims[4] - plot_dims[3]) * 0.033
  x_panel <- plot_dims[1] + (plot_dims[2] - plot_dims[1]) * 0.02
  text(x = x_panel, y = y_panel, adj = c(0.5, 0.5), panel_text, cex = 1.3)
  title_text <- c(sprintf('%s', get_model_type(model_id)), 'test error (RMSE °C)')
  if (model_id == 'wtemp_ERA5'){
    title_text[1L] <- paste0('*debiased ', title_text[1L])
  }
  add_map_legend(plot_dims, bin_breaks, n_cols, col_fun = viridis::inferno, col_fun_dir = 1L,
                 title = title_text)
  par(old_par)
}

#' plot number of observed lakes within each cell
plot_spatial_coverage <- function(metadata_fl, preds_obs_fl, cellsize, panel_text){

  plot_proj <- get_proj()
  pred_obs <- read_csv(preds_obs_fl, col_types = 'cDdddd')

  sites_sf <- read.csv(metadata_fl) %>%
    filter(num_obs > 0) %>%
    st_as_sf(coords = c("lake_lon_deg", "lake_lat_deg"), crs = 4326)

  site_grid <- st_make_grid(sites_sf, cellsize = cellsize, square = TRUE, offset = c(-125, 25)) %>%
    st_as_sf() %>%
    select(geometry=x) %>%
    mutate(cell_id = row_number()) %>%
    st_transform(plot_proj)


  # color bin breaks
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

  old_par <- par(mai = c(0.03,0.03,0.03,0.03), xpd = NA)

  plot(usa_sf, col = NA, border = NA, reset = FALSE, setParUsrBB = TRUE)
  plot(st_geometry(styled_grid), col = styled_grid$col, border = styled_grid$col, add = TRUE, lwd = 0.25)
  plot(usa_sf, col = NA, border = 'grey80', add = TRUE)
  plot_dims <- par('usr')
  y_panel <- plot_dims[4] - (plot_dims[4] - plot_dims[3]) * 0.033
  x_panel <- plot_dims[1] + (plot_dims[2] - plot_dims[1]) * 0.02
  text(x = x_panel, y = y_panel, adj = c(0.5, 0.5), panel_text, cex = 1.3)
  add_map_legend(plot_dims, bin_breaks, n_cols,
                 col_fun = viridis::mako, col_fun_dir = -1L,
                 title = 'Number of observed lakes (#)',
                 total_leg_prc = 0.42)

  par(old_par)
}


add_map_legend <- function(plot_dims, bin_breaks, n_cols, col_fun, col_fun_dir, title, total_leg_prc = 0.3){
  bin_w_prc <- total_leg_prc / n_cols
  bin_h_prc <- 0.045
  bin_w <- (plot_dims[2] - plot_dims[1]) * bin_w_prc
  total_leg_w <- (plot_dims[2] - plot_dims[1]) * total_leg_prc
  bin_h <- (plot_dims[4] - plot_dims[3]) * bin_h_prc
  x0 <- plot_dims[1] + (plot_dims[2] - plot_dims[1]) * 0.01
  y0 <- plot_dims[3] + (plot_dims[4] - plot_dims[3]) * 0.025
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
        text(x = this_x0 + bin_w/2, y = y0+bin_h, text_str, pos = 3, offset = 0.25, cex = 0.85)
      }
    } else {
      if (bin_breaks[i] %% 1 == 0 && bin_breaks[i] != 0){
        text(x = this_x0, y = y0+bin_h, bin_breaks[i], pos = 3, offset = 0.25, cex = 1)
      }
    }
  }
  text(x = x0, y = y0+bin_h *2.3, tail(title, 1L), pos = 4, offset = 0, cex = 1.2)
  if (length(title) == 2){
    text(x = x0, y = y0+bin_h * 3.3, title[1L], pos = 4, offset = 0, cex = 1.2)
  }


}


#' create an accuracy grid for a 1:1 scatter plot
#' use heat/intensity to indicate how many values are in that cell
plot_accuracy <- function(preds_obs_fl, cellsize, model_id, panel_text){

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
  preds_data <- read_csv(preds_obs_fl, col_types = 'cDdddd') %>%
    filter(!is.na(!!rlang::sym(model_id)))

  acc_vals <- preds_data %>%
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
       ylab = sprintf("%s-predicted surface temperature (°C)", get_model_type(model_id)),
       xlab = "Observed surface temperature (°C)", axes = FALSE,
       ylim = c(0, 38), xlim = c(0, 38))
  box()
  axis(1, at = seq(0, 40, by = 5), tck = -0.01)
  axis(2, at = seq(0, 40, by = 5), las = 1, tck = -0.01)
  abline(0,1, col = 'grey30', lwd = 0.5)
  abline(0,1, lty = 'dashed')
  if (model_id == "wtemp_ERA5"){
    abline(-3.47,1, lty = "dotted")
  }
  plot_dims <- par('usr')
  y_panel <- plot_dims[4] - (plot_dims[4] - plot_dims[3]) * 0.033
  y_rmse <- plot_dims[4] - (plot_dims[4] - plot_dims[3]) * 0.073
  y_rmse2 <- plot_dims[4] - (plot_dims[4] - plot_dims[3]) * 0.133
  x_panel <- plot_dims[1] + (plot_dims[2] - plot_dims[1]) * 0.039
  text(x = x_panel, y = y_panel, adj = c(0.5, 0.5), panel_text, cex = 1.3)

  text(x = x_panel, y = y_rmse, sprintf("RMSE: %s", round(rmse(preds_data[[model_id]], preds_data$wtemp_obs),2)), pos = 4)
  if (model_id == "wtemp_ERA5"){
    text(x = x_panel, y = y_rmse2, sprintf("*RMSE: %s", round(rmse(preds_data[[model_id]]+3.47, preds_data$wtemp_obs),2)), pos = 4)
  }
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

plot_coverage_panel <- function(fileout, centroids_sf, metadata_fl, preds_obs_fl, cellsize){

  png(file = fileout, width = 4.55, height = 5.5, units = 'in', res = 250)
  par(omi = c(0,0.05,0.05,0.05), mai = c(0,0,0,0), las = 1, xaxs = 'i', yaxs = 'i')
  panel_chars <- paste0(letters, ')')
  layout(matrix(c(1,1,1, 2,2), ncol = 1, byrow = TRUE))

  plot_spatial_coverage(metadata_fl = metadata_fl, preds_obs_fl = preds_obs_fl, cellsize = cellsize, panel_text = panel_chars[1L])
  panel_chars <- panel_chars[-1L]
  plot_data_coverage(centroids_sf = centroids_sf, preds_obs_fl = preds_obs_fl, panel_text = panel_chars[1L])


  dev.off()
}

plot_space_raw_panel <- function(fileout, metadata_fl, preds_obs_fl,
                                 accuracy_cellsize = 0.5,
                                 space_cellsize = 1,
                                 model_ids){
  png(file = fileout, width = 7.55, height = 9.1, units = 'in', res = 250)
  par(omi = c(0,0.05,0.05,0.05), mai = c(0,0,0,0), las = 1, xaxs = 'i', yaxs = 'i')
  panel_chars <- paste0(letters, ')')
  layout(matrix(c(1,1,1, 2,2,3,3,3,4,4,5,5,5,6,6), nrow = 3, byrow = TRUE))

  for (j in 1:3){

    plot_spatial_accuracy(metadata_fl, preds_obs_fl,
                          cellsize = space_cellsize,
                          model_id = model_ids[j],
                          panel_text = panel_chars[1L])
    panel_chars <- panel_chars[-1L]
    plot_accuracy(preds_obs_fl,
                  cellsize = accuracy_cellsize,
                  model_id = model_ids[j],
                  panel_text = panel_chars[1L])
    panel_chars <- panel_chars[-1L]
  }

  dev.off()

}
