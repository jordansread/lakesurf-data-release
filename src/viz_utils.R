

plot_time_season_accuracy <- function(fileout, preds_obs_fl){

  spring_col <- "#1B9E77"
  summer_col <- "#D95F02"
  fall_col <- "#E7298A"
  winter_col <- "#7570B3"

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
      season == 'spring' ~ spring_col,
      season == 'summer' ~ summer_col,
      season == 'fall' ~ fall_col,
      season == 'winter' ~ winter_col
    ))

  png(file = fileout, width = 10, height = 6, units = 'in', res = 250)
  par(omi = c(0,0,0.05,0.05), mai = c(0.5,1,0,0), las = 1, mgp = c(2,.5,0), cex = 1.5)

  plot(NA, NA, xlim = c(1980, 2020), ylim = c(0.6, 1.4),
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
  spring_col <- "#1B9E77"
  summer_col <- "#D95F02"
  fall_col <- "#E7298A"
  winter_col <- "#7570B3"

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
      season == 'spring' ~ spring_col,
      season == 'summer' ~ summer_col,
      season == 'fall' ~ fall_col,
      season == 'winter' ~ winter_col
    ))

  png(file = fileout, width = 10, height = 4, units = 'in', res = 250)
  par(omi = c(0,0,0.05,0.05), mai = c(0.5,1,0,0), las = 1, mgp = c(2,.5,0), cex = 1.5)

  plot(NA, NA, xlim = c(1980, 2020), ylim = c(0, 17000),
       ylab = "Suface temperature observations (#)", xlab = "", axes = FALSE)

  axis(1, at = seq(1970, 2030, by = 5), tck = -0.01)
  axis(2, at = seq(0,20000, by = 5000), labels = paste(seq(0, 20, by = 5), 'k', sep = ''), las = 1, tck = -0.01)

  for (year in 1980:2020){
    winter_ht <- filter(plot_data, year == !!year, season == 'winter') %>% pull(n)
    spring_ht <- filter(plot_data, year == !!year, season == 'spring') %>% pull(n)
    summer_ht <- filter(plot_data, year == !!year, season == 'summer') %>% pull(n)
    fall_ht <- filter(plot_data, year == !!year, season == 'fall') %>% pull(n)

    rect(year-0.5, ybottom = 0, xright = year+0.5, ytop = winter_ht, col = winter_col, border = NA)
    rect(year-0.5, ybottom = winter_ht, xright = year+0.5, ytop = winter_ht + spring_ht, col = spring_col, border = NA)
    rect(year-0.5, ybottom = winter_ht + spring_ht, xright = year+0.5, ytop = winter_ht + spring_ht + summer_ht, col = summer_col, border = NA)
    rect(year-0.5, ybottom = winter_ht + spring_ht + summer_ht, xright = year+0.5, ytop = winter_ht + spring_ht + summer_ht + fall_ht, col = fall_col, border = NA)
  }
  dev.off()
}

plot_spatial_accuracy <- function(fileout, metadata_fl, preds_obs_fl, cellsize){

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

  col_tbl <- tibble(val = seq(0, 20, by = 0.5),
         col = c(viridis(n = 9), rep(tail(viridis(n = 9), 1L), times = 32)),
         bin = cut(val, breaks = seq(0, 20, by = 0.5), right = F)) %>%
    select(-val)

  cell_obs <- st_transform(sites_sf, plot_proj) %>%
    st_intersection(site_grid, .) %>%
    st_drop_geometry() %>% select(cell_id, site_id) %>%
    right_join(pred_obs, by = 'site_id') %>%
    group_by(cell_id) %>%
    summarize(rmse = sqrt(mean((wtemp_EALSTM - wtemp_obs)^2, na.rm=TRUE)),
              n = sum(!is.na(wtemp_EALSTM))) %>%
    filter(n >= min_obs) %>%
    mutate(bin = cut(rmse, breaks = seq(0, 20, by = 0.5), right = F)) %>%
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
