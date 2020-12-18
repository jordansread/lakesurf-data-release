

sf_to_zip <- function(zip_filename, sf_object, layer_name){
  cdir <- getwd()
  on.exit(setwd(cdir))
  dsn <- tempdir()

  sf::st_write(sf_object, dsn = dsn, layer = layer_name, driver="ESRI Shapefile", delete_dsn=TRUE) # overwrites

  files_to_zip <- data.frame(filepath = dir(dsn, full.names = TRUE), stringsAsFactors = FALSE) %>%
    mutate(filename = basename(filepath)) %>%
    filter(str_detect(string = filename, pattern = layer_name)) %>% pull(filename)

  setwd(dsn)
  zip(file.path(cdir, zip_filename), files = files_to_zip)
  setwd(cdir)
}

extract_model_ids <- function(filepath){
  read_csv(filepath) %>% pull(site_id) %>% unique()
}

create_metadata_file <- function(fileout, sites, table, lakes_sf, lat_lon_fl, meteo_fl, gnis_names_fl){
  sdf <- sf::st_transform(lakes_sf, 2811) %>%
    mutate(perim = lwgeom::st_perimeter_2d(Shape), area = sf::st_area(Shape), circle_perim = 2*pi*sqrt(area/pi), SDF = perim/circle_perim) %>%
    sf::st_drop_geometry() %>% select(site_id, SDF)

  sites %>% inner_join((readRDS(lat_lon_fl)), by = 'site_id') %>%
    inner_join(sdf, by = 'site_id') %>%
    rename(centroid_lon = longitude, centroid_lat = latitude) %>%
    inner_join(table, by = 'site_id') %>%
    inner_join(readRDS(meteo_fl), by = 'site_id') %>%
    inner_join((readRDS(gnis_names_fl)), by = 'site_id') %>% rename(lake_name = GNIS_Name, meteo_filename = meteo_fl) %>%
    write_csv(fileout)

}

#' hack to lock in data from Dec 2020
write_locked_data_subset <- function(){

  cd <- getwd()
  # only include lakes that were modeled w/ GLM2 and are in the hyperscales release
  setwd('../hyperscales-data-release/')
  mod_lake_ids <- remake::fetch('modeled_lake_ids')
  setwd(cd)

  surf_data <- feather::read_feather('../lake-temperature-model-prep/7b_temp_merge/out/temp_data_with_sources.feather') %>%
    filter(depth <= 1, site_id %in% mod_lake_ids) %>% group_by(site_id, date) %>%
    summarize(temp = mean(temp), depth = 'surf', .groups = 'drop')

  # now drop lakes w/ less than 10 obs total
  keep_ids <- surf_data %>% group_by(site_id) %>% tally() %>% filter(n >= 10) %>% pull(site_id)

  surf_data %>% filter(site_id %in% keep_ids) %>%
    write_csv('in_data/surface_temperature_obs.csv')
}
