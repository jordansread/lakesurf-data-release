
convert_feather_file <- function(fileout, feather_fl){

  arrow::read_feather(feather_fl) %>%
    rename(wtemp_obs = wtemp) %>%
    write_csv(fileout)
}

sf_centroid_metadata <- function(filein){
  read_csv(filein) %>% st_as_sf(coords = c("lon", "lat"),
           crs = 4326)
}

convert_preds_tibble <- function(filein){

  read_csv(filein, col_types = "--Dcddd") %>%
    select(site_id, Date,
           wtemp_EALSTM = `wtemp_predicted-ealstm`,
           wtemp_LM = `wtemp_predicted-linear_model`,
           wtemp_obs = wtemp_actual)
}

# in text info:
# compare specific lakes
# read_csv('out_data/lake_surface_temp_preds.csv') %>%
#   group_by(site_id) %>%
#   summarize(`*RMSE_ERA5` = sqrt(mean((wtemp_ERA5 + 3.31 - wtemp_obs)^2, na.rm = TRUE)),
#             RMSE_ERA5 = sqrt(mean((wtemp_ERA5 - wtemp_obs)^2, na.rm = TRUE)),
#             RMSE_EALSTM = sqrt(mean((wtemp_EALSTM - wtemp_obs)^2, na.rm = TRUE)),
#             RMSE_LM = sqrt(mean((wtemp_LM - wtemp_obs)^2, na.rm = TRUE)),
#             .groups = 'drop') %>%
#   mutate(RMSE_LM = ifelse(is.na(RMSE_LM), 99, RMSE_LM),
#          RMSE_ERA5 = ifelse(is.na(RMSE_ERA5), 99, RMSE_ERA5),
#          `*RMSE_ERA5` = ifelse(is.na(`*RMSE_ERA5`), 99, `*RMSE_ERA5`),
#          is_best_raw = RMSE_EALSTM <= RMSE_LM & RMSE_EALSTM <= RMSE_ERA5,
#          is_best_deb = RMSE_EALSTM <= RMSE_LM & RMSE_EALSTM <= `*RMSE_ERA5`) %>%
#   summarize(n_best_raw = sum(is_best_raw),
#             prc_best_raw = sum(is_best_raw) / length(is_best_raw),
#             n_best_deb = sum(is_best_deb),
#             prc_best_deb = sum(is_best_deb) / length(is_best_raw))
# for SPATIAL CELLS, see viz_utils, `plot_spatial_accuracy()`
# for ERA5 CELL COUNT, see
# compare specific years:
# read_csv('out_data/lake_surface_temp_preds.csv') %>%
#   mutate(year = lubridate::year(Date)) %>%
#   group_by(year) %>%
#   summarize(`*RMSE_ERA5` = sqrt(mean((wtemp_ERA5 + 3.31 - wtemp_obs)^2, na.rm = TRUE)),
#             RMSE_ERA5 = sqrt(mean((wtemp_ERA5 - wtemp_obs)^2, na.rm = TRUE)),
#             RMSE_EALSTM = sqrt(mean((wtemp_EALSTM - wtemp_obs)^2, na.rm = TRUE)),
#             RMSE_LM = sqrt(mean((wtemp_LM - wtemp_obs)^2, na.rm = TRUE)),
#             .groups = 'drop') %>%
#   mutate(is_best_raw = RMSE_EALSTM <= RMSE_LM & RMSE_EALSTM <= RMSE_ERA5,
#          is_best_deb = RMSE_EALSTM <= RMSE_LM & RMSE_EALSTM <= `*RMSE_ERA5`) %>%
#   summarize(n_best_raw = sum(is_best_raw),
#             prc_best_raw = sum(is_best_raw) / length(is_best_raw),
#             n_best_deb = sum(is_best_deb),
#             prc_best_deb = sum(is_best_deb) / length(is_best_raw))
# Compare median lake-specific rmse:
# read_csv('out_data/lake_surface_temp_preds.csv') %>% group_by(site_id) %>%
#   summarize(`*RMSE_ERA5` = sqrt(mean((wtemp_ERA5 + 3.31 - wtemp_obs)^2, na.rm = TRUE)),
#     RMSE_ERA5 = sqrt(mean((wtemp_ERA5 - wtemp_obs)^2, na.rm = TRUE)),
#             RMSE_EALSTM = sqrt(mean((wtemp_EALSTM - wtemp_obs)^2, na.rm = TRUE)),
#             RMSE_LM = sqrt(mean((wtemp_LM - wtemp_obs)^2, na.rm = TRUE)),
#             .groups = 'drop') %>% ungroup() %>%
#   summarize(`*med_RMSE_ERA` = median(`*RMSE_ERA5`, na.rm = TRUE),
#     med_RMSE_ERA5 = median(RMSE_ERA5, na.rm = TRUE),
#             med_RMSE_EALSTM = median(RMSE_EALSTM, na.rm = TRUE),
#             med_RMSE_LM = median(RMSE_LM, na.rm = TRUE))

add_source_info_obs <- function(fileout, obs_pred_fl, source_fl){

  obs_all <- read_csv(source_fl, show_col_types = FALSE) %>%
    # we don't use this information, but I verified it was identical to the other file
    # max difference of 1.831055e-06°C
    select(-wtemp_obs)

  wqp_orgs <- filter(obs_all, !str_detect(source, 'ALABAMACOUSHATTATRIBE.TX_WQX') & !str_detect(source, '.rds')) %>%
    select(Date, site_id, source) %>% pull(source) %>% unique() %>%
    dataRetrieval::whatWQPsites(siteid = .) %>%
    select(obs_data_source = OrganizationFormalName, source = MonitoringLocationIdentifier)


  read_csv(obs_pred_fl, show_col_types = FALSE) %>%
    # obs_pred is a subset of source/obs_all, since the latter doesn't have the ERA5-based QAQC
    left_join(obs_all, by = c('Date','site_id')) %>%
    left_join(wqp_orgs, by = 'source') %>%
    mutate(obs_data_source = replace_na(obs_data_source, 'unresolved')) %>%
    select(site_id, Date, wtemp_EALSTM, wtemp_LM, wtemp_ERA5, wtemp_obs, obs_data_source) %>%
    write_csv(file = fileout)

}

match_era5_grid2obs <- function(fileout, obs_pred, nc_fl, centroids_sf, cell_res){
  # create a grid for the ERA5 data, which is gridded on a 0.25° or 0.1° lat/lon grid. Will use this to match lakes to the grid
  era5_grid <- sf_grid_nc(nc_fl, cell_res = cell_res)
  # see https://confluence.ecmwf.int/pages/viewpage.action?pageId=173385064 for info on this dimension

  #expver <- 1 #not used in 0.1° ERA5
  # add a row column so we know how to reassemble
  obs_pred <- mutate(obs_pred, Date = as.character(Date), row_num = row_number(),
                     wtemp_ERA5 = NA_real_)

  lake_pts <- centroids_sf %>%
    filter(site_id %in% obs_pred$site_id)
  # assign netcdf cell indices to each lake in the dataset,
  # which happens below when we use obs_pred[replace_data$row_num, ] <- replace_data
  era_cell_indices <- feature_cell_indices(cell_grid = era5_grid, lake_pts)

  # to get the count of unique ERA5 cells that have observations, use this:
  # era_cell_indices %>% group_by(x, y) %>% filter(row_number(site_id) == 1) %>% nrow()

  # to get the count of unique ERA5 cells that have _lakes_ in this dataset, use this:
  # feature_cell_indices(cell_grid = era5_grid, centroids_sf) %>% group_by(x, y) %>% filter(row_number(site_id) == 1) %>% nrow()

  nc <- ncdf4::nc_open(nc_fl)

  # relies on this file having units "hours since 1900-01-01 00:00:00.0"
  # verified this is the case for ERA5 0.25° and 0.1° data
  nc_time <- as.character(as.Date('1900-01-01') + ncdf4::ncvar_get(nc, 'time')/24)

  un_indices <- select(era_cell_indices, -site_id) %>% filter(!duplicated(.))
  for (j in 1:nrow(un_indices)){

    this_x <- un_indices[j,]$x
    this_y <- un_indices[j,]$y
    these_sites <- filter(era_cell_indices, x == this_x, y == this_y) %>%
      pull(site_id)

    these_data <- obs_pred %>% filter(site_id %in% these_sites) %>% select(-wtemp_ERA5)
    # convert to celsius and access this single cell
    replace_data <- tibble(Date = nc_time,
                           wtemp_ERA5 = ncdf4::ncvar_get(
             nc, 'lmlt',
             start = c(this_x, this_y, 1L),
             count = c(1L, 1L, -1L))- 273.15) %>%
      left_join(these_data, ., by = "Date")
    obs_pred[replace_data$row_num, ] <- replace_data

  }
  ncdf4::nc_close(nc)
  obs_pred %>% mutate(Date = as.Date(Date)) %>% select(-row_num) %>%
    write_csv(fileout)
}

rmse <- function(pred, obs){
  sqrt(mean((pred - obs)^2, na.rm=TRUE))
}

calc_site_errors <- function(filein){
  read_csv(filein, col_types = 'cDdddd') %>%
    group_by(site_id) %>% summarize(
      RMSE_EALSTM = rmse(wtemp_EALSTM, wtemp_obs),
      RMSE_LM = rmse(wtemp_LM, wtemp_obs),
      RMSE_ERA5 = rmse(wtemp_ERA5, wtemp_obs),
      num_obs = length(Date))

}
build_metadata <- function(fileout, orig_meta_fl, release_grid_sf, weather_centroids, error_data, cluster_fl){

  cluster_data <- read_csv(cluster_fl, col_types = '--c-----------d--d')

  orig_metadata <- read_csv(orig_meta_fl)

  orig_metadata %>% st_as_sf(coords = c("lon", "lat"),
                             crs = 4326) %>%
    assign_group_id(polygons = release_grid_sf, use_col = "site_id") %>%
    inner_join(orig_metadata, by = 'site_id') %>%
    inner_join(weather_centroids, by = c('x','y')) %>%
    left_join(error_data) %>%
    left_join(cluster_data) %>%
    # now sort
    arrange(group_id, x, desc(y)) %>%
    mutate(weather_id = sprintf('nldas_x%s_y%s', x, y)) %>%
    mutate(num_obs = case_when(
      is.na(num_obs) ~ 0L,
      TRUE ~ num_obs
    )) %>%
    select(site_id, weather_id, num_obs, area_m2,
           elevation_m = elevation, lake_lon_deg = lon, lake_lat_deg = lat,
           weather_lon_deg = lon_cell, weather_lat_deg = lat_cell, num_obs,
           x, y, RMSE_EALSTM, RMSE_LM, RMSE_ERA5, cluster_id, fold_id, group_bbox, group_id)
}

subset_write <- function(fileout, tbl, remove_cols){
  select(tbl, -one_of(remove_cols)) %>%
    write_csv(file = fileout)
}

subset_tbl <- function(tbl, ...){
  select(tbl, ...) %>%
    unique()
}

combine_hash_files <- function(hash_out, ...){
  hash_files <- c(...)
  lapply(hash_files, yaml::yaml.load_file) %>% unlist() %>% as.list() %>%
    yaml::write_yaml(hash_out)
}
