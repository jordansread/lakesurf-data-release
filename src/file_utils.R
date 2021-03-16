
convert_feather_file <- function(fileout, feather_fl){

  arrow::read_feather(feather_fl) %>%
    write_csv(fileout)
}

sf_centroid_metadata <- function(filein){
  read_csv(filein) %>% st_as_sf(coords = c("lake_lon_deg", "lake_lat_deg"),
           crs = 4326)
}

build_metadata <- function(fileout, orig_meta_fl, release_grid_sf, weather_centroids){

  orig_metadata <- read_csv(orig_meta_fl)

  orig_metadata %>% st_as_sf(coords = c("lon", "lat"),
                             crs = 4326) %>%
    assign_group_id(polygons = release_grid_sf, use_col = "site_id") %>%
    inner_join(orig_metadata, by = 'site_id') %>%
    inner_join(weather_centroids, by = c('x','y')) %>%
    # now sort
    arrange(group_id, x, desc(y)) %>%
    mutate(predict_id = row_number()) %>% group_by(group_id, x, desc(y)) %>%
    mutate(weather_id = cur_group_id()) %>% ungroup() %>%
    select(predict_id, weather_id, site_id, group_id, area_m2,
           elevation_m = elevation, lake_lon_deg = lon, lake_lat_deg = lat,
           weather_lon_deg = lon_cell, weather_lat_deg = lat_cell, observed, x, y, group_bbox)
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
