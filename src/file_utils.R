
convert_feather_file <- function(fileout, feather_fl){

  arrow::read_feather(feather_fl) %>%
    rename(wtemp_obs = wtemp) %>%
    write_csv(fileout)
}

sf_centroid_metadata <- function(filein){
  read_csv(filein) %>% st_as_sf(coords = c("lake_lon_deg", "lake_lat_deg"),
           crs = 4326)
}

convert_preds_file <- function(fileout, filein){
  read_csv(filein) %>%
    select(site_id,
           Date,
           wtemp_EALSTM = `wtemp_predicted-ealstm`,
           wtemp_XGB = `wtemp_predicted-xgboost`,
           wtemp_LM = `wtemp_predicted-linear_model`,
           wtemp_obs = wtemp_actual) %>%
    write_csv(file = fileout)
}

build_metadata <- function(fileout, orig_meta_fl, release_grid_sf, weather_centroids, error_fl, cluster_fl){


  error_data <- read_csv(error_fl) %>%
    select(site_id, num_obs = n_obs, RMSE_EALSTM = rmse_ealstm, RMSE_XGB = rmse_xgboost, RMSE_LM = rmse_lm)
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
           x, y, RMSE_EALSTM, RMSE_XGB, RMSE_LM, cluster_id, group_bbox, group_id)
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
