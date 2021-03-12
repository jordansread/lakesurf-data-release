
sf_centroid_metdata <- function(metadata_fl){
  readr::read_csv(metadata_fl) %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = 4326)
}


create_site_group_grid <- function(centroids_sf, box_res){
  bbox_grid <- sf::st_make_grid(centroids_sf, square = TRUE, cellsize = box_res, offset = c(-126,23)) %>%
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
create_ldas_centroids <- function(x0, y0, x_num, y_num, cell_res, filter_to){
  ldas_crs <- "+init=epsg:4326"

  ldas_grid_sfc <- sf::st_make_grid(cellsize = cell_res, n = c(x_num, y_num),
                                    offset = c(x0-cell_res/2, y0-cell_res/2), crs = ldas_crs)
  # cells count left to right, then next row, then left to right
  x_cells <- rep(0:(x_num-1), y_num)
  y_cells <- c(sapply(0:(y_num-1), function(x) rep(x, x_num)))

  st_sf(data.frame(cell_id = paste('x_',x_cells, '_y_',y_cells, sep = '')), ldas_grid_sfc) %>%
    filter(cell_id %in% filter_to$cell_id) %>% st_centroid()
}
