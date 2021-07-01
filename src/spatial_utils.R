

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


create_site_group_grid <- function(box_res, offset = c(-126,23)){
  bbox_grid <- sf::st_make_grid(centroids_sf, square = TRUE, cellsize = box_res, offset = offset) %>%
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

generate_group_rects <- function(){
  unit_cell <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,-1), c(0,-1), c(0,0))))
  shift_scale <- function(shift = c(0,0), scale = 1){
    (unit_cell * scale)+shift
  }
  group_rects <- st_sfc(crs = "+init=epsg:4326",
                        shift_scale(c(-126,53), c(28,13)),
                        shift_scale(c(-126,40), c(28,16)),
                        shift_scale(c(-98, 53), c(16,13)),
                        shift_scale(c(-98, 40), c(16,16)),
                        shift_scale(c(-82, 53), c(15,29)))

  groups <- data.frame(group_id = rep(NA_character_, length(group_rects)), stringsAsFactors = FALSE)
  for (i in 1:length(group_rects)){
    # format of N46.125-48.625_W86.5-89.25
    this_box <- st_bbox(group_rects[i])
    groups$group_id[i] <- sprintf("%02d", i)
    groups$group_bbox[i] <- sprintf("N%1.0f-%1.0f_W%1.0f-%1.0f", this_box$ymin, this_box$ymax, -this_box$xmax, -this_box$xmin)
  }

  return(st_sf(groups, group_rects))

}


#' create a polygon cell grid from specifications. Assumes lat/lon data
#' and rectangular polygons.
#'
#' @param x0 the left edge of the cell grid to be built
#' @param y0 the lower edge of the cell grid to be built
#' @param x_num the number of cells in the x dimension
#' @param y_num the number of cells in the y dimension
#' @param cell_res the resolution (width and height) of the cells
#'
#' @return an sf data.frame with x and y attributes, specifying
#' cell indices (0 indexed)
ldas_centroid_lat_lon <- function(x0, y0, x_num, y_num, cell_res){
  ldas_crs <- "+init=epsg:4326"

  ldas_grid_sfc <- sf::st_make_grid(cellsize = cell_res, n = c(x_num, y_num),
                                    offset = c(x0-cell_res/2, y0-cell_res/2), crs = ldas_crs,
                                    what = 'centers')
  # cells count left to right, then next row, then left to right
  x_cells <- rep(0:(x_num-1), y_num)
  y_cells <- c(sapply(0:(y_num-1), function(x) rep(x, x_num)))

  st_sf(data.frame(x = x_cells, y = y_cells), ldas_grid_sfc) %>%
    dplyr::mutate(lat_cell = sf::st_coordinates(.)[,2],
                  lon_cell = sf::st_coordinates(.)[,1]) %>%
    st_drop_geometry()
}
