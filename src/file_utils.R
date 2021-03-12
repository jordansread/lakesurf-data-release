
convert_feather_file <- function(fileout, feather_fl){

  arrow::read_feather(feather_fl) %>%
    write_csv(fileout)
}

update_metadata_file <- function(fileout, orig_meta_fl){

  read_csv(orig_meta_fl) %>% mutate(cell_id = paste('x_',x, '_y_',y, sep = '')) %>%
    select(site_id, cell_id, area_m2, elevation_m = elevation, lon, lat, observed) %>%
    write_csv(fileout)
}
