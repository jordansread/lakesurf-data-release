
sf_centroid_metdata <- function(metadata_fl){
  readr::read_csv(metadata_fl) %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = 4326)
}


create_site_group_grid <- function(centroids_sf, box_res){
  bbox_grid <- sf::st_make_grid(centroids_sf, square = TRUE, cellsize = box_res, offset = c(-169,11)) %>%
    st_sf(group_id = paste0('group_', 1:length(.)))

  # write file of buffers that fit in each box
  # rows are centroids, columns are boxes. Contents are logicals:
  box_df <- st_within(centroids_sf, bbox_grid, sparse = F) %>%
    as_tibble(.name_repair = function(x){paste0('group_', seq_len(length(x)))}) %>%
    summarize_all(sum) %>%
    pivot_longer(cols = starts_with('group_'), names_to = 'group_id', values_to = 'site_total') %>%
    filter(site_total > 0)

  bbox_grid %>% inner_join(box_df)
}
