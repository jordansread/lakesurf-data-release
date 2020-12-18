subset_lake_sf <- function(lakes_sf_fl, site_ids){
  readRDS(lakes_sf_fl) %>% filter(site_id %in% !!site_ids) %>% sf::st_zm()
}


fetch_zip_url_sf <- function(zip_url, layer_name){

  destination = tempfile(pattern = layer_name, fileext='.zip')
  file <- GET(zip_url, write_disk(destination, overwrite=T), progress())
  shp_path <- tempdir()
  unzip(destination, exdir = shp_path)

  sf::st_read(shp_path, layer=layer_name) %>%
    st_transform(crs = 4326) %>%
    mutate(state = dataRetrieval::stateCdLookup(STATEFP)) %>%
    dplyr::select(state, county = NAME)

}


sf_names_from_overlap <- function(sf_polys1, sf_polys2){

  # will be some NA when the point isn't contained within the polgyon:
  match_idx <- st_transform(sf_polys1, crs = "+init=epsg:2811") %>% sf::st_simplify(dTolerance = 40) %>%
    st_transform(crs = st_crs(sf_polys2)) %>%
    st_intersects(sf_polys2)

  stopifnot(sum(sapply(match_idx, is.null)) == 0) # check that all poly1 have matches
  purrr::map(1:nrow(sf_polys1),function(i){
    info <- st_drop_geometry(sf_polys2[match_idx[[i]],])
    cbind(st_drop_geometry(sf_polys1[i,]),
          lapply(info, function(x) { # lapply across the data.frame columns, squash/combine values when > 1
            paste(sort(unique(x)), collapse = '|')
          }) %>% data.frame(stringsAsFactors = FALSE))
  }) %>% purrr::reduce(bind_rows)
}
