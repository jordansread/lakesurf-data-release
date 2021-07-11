
convert_feather_file <- function(fileout, feather_fl){

  arrow::read_feather(feather_fl) %>%
    rename(wtemp_obs = wtemp) %>%
    write_csv(fileout)
}

sf_centroid_metadata <- function(filein){
  read_csv(filein) %>% st_as_sf(coords = c("lake_lon_deg", "lake_lat_deg"),
           crs = 4326)
}

convert_preds_tibble <- function(filein){
  read_csv(filein) %>%
    select(site_id,
           Date,
           wtemp_EALSTM = `wtemp_predicted-ealstm`,
           wtemp_LM = `wtemp_predicted-linear_model`,
           wtemp_obs = wtemp_actual)
}

match_era5_grid2obs <- function(fileout, obs_pred, nc_fl, obs_metadata){
  # create a grid for the ERA5 data, which is gridded on a 0.25° lat/lon grid. Will use this to match lakes to the grid
  era5_grid <- sf_grid_nc(nc_fl)

  # see https://confluence.ecmwf.int/pages/viewpage.action?pageId=173385064 for info on this dimension
  expver <- 1
  # add a row column so we know how to reassemble?
  obs_pred <- mutate(obs_pred, Date = as.character(Date), row_num = row_number(),
                     wtemp_ERA5 = NA_real_)

  lake_pts <- st_as_sf(obs_metadata,
                       coords = c("lake_lon_deg", "lake_lat_deg"),
                       crs = 4326) %>%
    filter(site_id %in% obs_pred$site_id)
  # assign netcdf cell indices to each lake in the dataset:
  era_cell_indices <- feature_cell_indices(cell_grid = era5_grid, lake_pts)

  nc <- ncdf4::nc_open(nc_fl)

  # relies on this file having units "hours since 1900-01-01 00:00:00.0"
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
             start = c(this_x, this_y, expver, 1L),
             count = c(1L, 1L, 1L, -1L))- 273.15) %>%
      left_join(these_data, ., by = "Date")
    obs_pred[replace_data$row_num, ] <- replace_data

  }
  ncdf4::nc_close(nc)
  obs_pred %>% mutate(Date = as.Date(Date)) %>% select(-row_num) %>%
    write_csv(fileout)
}

build_metadata <- function(fileout, orig_meta_fl, release_grid_sf, weather_centroids, error_fl, cluster_fl){


  error_data <- read_csv(error_fl) %>%
    select(site_id, num_obs = n_obs, RMSE_EALSTM = rmse_ealstm, RMSE_LM = rmse_lm)
  cluster_data <- read_csv(cluster_fl) %>%
    select(site_id, cluster_id = `5fold_fold`) %>%
    mutate(cluster_id = cluster_id + 1)

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
      is.na(num_obs) ~ 0,
      TRUE ~ num_obs
    )) %>%
    select(site_id, weather_id, num_obs, area_m2,
           elevation_m = elevation, lake_lon_deg = lon, lake_lat_deg = lat,
           weather_lon_deg = lon_cell, weather_lat_deg = lat_cell, num_obs,
           x, y, RMSE_EALSTM, RMSE_LM, cluster_id, group_bbox, group_id)
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
