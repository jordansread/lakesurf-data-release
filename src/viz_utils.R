
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
    summarize(rmse = sqrt(mean((wtemp_EALSTM - wtemp_obs)^2, na.rm=TRUE))) %>%
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

plot_spatial_accuracy <- function(fileout, metadata_fl, preds_obs_fl, cellsize, model_id){

  min_obs <- 10 # minimum number of observations per cell to plot a color
  plot_proj <- "+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=999999.9999898402 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

  pred_obs <- read_csv(preds_obs_fl)

  sites_sf <- read.csv(metadata_fl) %>%
    filter(num_obs > 0) %>%
    st_as_sf(coords = c("lake_lon_deg", "lake_lat_deg"), crs = 4326)

  site_grid <- st_make_grid(sites_sf, cellsize = cellsize, square = TRUE, offset = c(-125, 25)) %>%
    st_as_sf() %>%
    select(geometry=x) %>%
    mutate(cell_id = row_number()) %>%
    st_transform(plot_proj)

  bin_breaks <- c(seq(0, 4, by = 0.5), 20)
  n_cols <- 9

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

  # cell_obs %>% arrange(desc(rmse)) # to find the worst offenders
  #
  # get all the predictions so you can filter by cell_id
  # pred_sites <- st_transform(sites_sf, plot_proj) %>%
  #   st_intersection(site_grid, .) %>%
  #   st_drop_geometry() %>% select(cell_id, site_id) %>%
  #   right_join(pred_obs, by = 'site_id')
  #
  # filter by cell_id
  # pred_sites %>% filter(cell_id == 877) %>% group_by(source) %>% summarize(rmse = sqrt(mean((wtemp_EALSTM - wtemp_obs)^2, na.rm=TRUE)))

  usa_sf <- sf::st_transform(spData::us_states, crs = 4326) %>%
    st_as_sf() %>%
    st_transform(plot_proj) %>% st_geometry()

  styled_grid <- site_grid %>% left_join(cell_obs, by = 'cell_id')

  png(file = fileout, width = 10, height = 6, units = 'in', res = 250)
  par(omi = c(0,0,0.00,0.00), mai = c(0,0,0,0))

  plot(st_geometry(styled_grid), col = styled_grid$col, border = styled_grid$col, reset = FALSE, lwd = 0.5)
  plot(usa_sf, col = NA, border = 'grey80', add = TRUE)
  dev.off()
}

#' plot number of observed lakes within each cell
plot_spatial_coverage <- function(fileout, metadata_fl, preds_obs_fl, cellsize){

  plot_proj <- "+proj=lcc +lat_1=30.7 +lat_2=29.3 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=999999.9999898402 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

  pred_obs <- read_csv(preds_obs_fl)

  sites_sf <- read.csv(metadata_fl) %>%
    filter(num_obs > 0) %>%
    st_as_sf(coords = c("lake_lon_deg", "lake_lat_deg"), crs = 4326)

  site_grid <- st_make_grid(sites_sf, cellsize = cellsize, square = TRUE, offset = c(-125, 25)) %>%
    st_as_sf() %>%
    select(geometry=x) %>%
    mutate(cell_id = row_number()) %>%
    st_transform(plot_proj)


  bin_breaks <- c(1, 2, 5, 10, 50, 100, 1000000)
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
  par(omi = c(0,0,0.00,0.00), mai = c(0,0,0,0))

  plot(st_geometry(styled_grid), col = styled_grid$col, border = styled_grid$col, reset = FALSE, lwd = 0.5)
  plot(usa_sf, col = NA, border = 'grey80', add = TRUE)
  dev.off()

}

#' create an accuracy grid for a 1:1 scatter plot
#' use heat/intensity to indicate how many values are in that cell
plot_accuracy <- function(fileout, preds_obs_fl, cellsize, model_id){

  acc_grid <- st_sfc(st_polygon(list(rbind(c(0,0), c(40,0), c(40,40), c(0,0))))) %>%
    st_make_grid(cellsize = cellsize) %>%
    st_as_sf() %>%
    select(geometry=x) %>%
    mutate(cell_id = row_number())

  bin_breaks <- c(1, seq(10, 500, by = 10), 10000000000)
  n_cols <- length(bin_breaks) - 1

  col_tbl <- tibble(val = bin_breaks,
                    col = c(viridis::mako(n = n_cols, direction = -1L),
                            # we want all values above a certain threshold to be the same color
                            rep(tail(viridis::mako(n = n_cols, direction = -1L), 1L),
                                times = length(bin_breaks) - n_cols)),
                    bin = cut(val, breaks = bin_breaks, right = F)) %>%
    filter(!is.na(bin)) %>%
    select(-val)


  # lat is y, lon is x. Lon comes first:
  acc_vals <- read_csv(preds_obs_fl) %>%
    # shrink this for exploration:
    #head(10000) %>%
    st_as_sf(coords = c("wtemp_obs", model_id)) %>%
    st_intersection(acc_grid, .) %>%
    st_drop_geometry() %>% select(cell_id) %>%
    group_by(cell_id) %>%
    summarize(val = length(cell_id)) %>%
    mutate(bin = cut(val, breaks = bin_breaks, right = F)) %>%
    left_join(col_tbl, by = 'bin')

  styled_grid <- acc_grid %>% left_join(acc_vals, by = 'cell_id')

  png(file = fileout, width = 10, height = 10, units = 'in', res = 250)
  par(omi = c(0,0,0.00,0.00), mai = c(1,1,0.1,0.1), las = 1, mgp = c(2,.5,0), cex = 1.5, xaxs = 'i', yaxs = 'i')
  plot(st_geometry(styled_grid), col = styled_grid$col, border = styled_grid$col, reset = FALSE,
       ylab = "Predicted surface temperature (°C)", xlab = "Observed surface temperature (°C)", axes = FALSE,
       ylim = c(0, 38), xlim = c(0, 38))
  box()
  axis(1, at = seq(0, 40, by = 5), tck = -0.01)
  axis(2, at = seq(0, 40, by = 5), las = 1, tck = -0.01)
  abline(0,1, lty = 'dashed')
  dev.off()

}
