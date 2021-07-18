

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

sf_grid_nc <- function(nc_file){
  nc <- ncdf4::nc_open(nc_file)

  lon <- ncdf4::ncvar_get(nc, 'longitude')
  lat <- ncdf4::ncvar_get(nc, 'latitude')

  ncdf4::nc_close(nc)

  x0 <- lon[1]
  y0 <- tail(lat, 1)

  cell_res <- unique(diff(lon)) # is the same for both

  stopifnot(cell_res == unique(-diff(lat)))

  x_num <- length(lon)
  y_num <- length(lat)

  x_cells <- rep(1:(x_num), y_num)
  y_cells <- c(sapply(y_num:1, function(x) rep(x, x_num)))

  sf::st_make_grid(cellsize = cell_res, n = c(x_num, y_num),
                   offset = c(x0-cell_res/2, y0-cell_res/2),
                   what = 'polygons') %>%
    st_as_sf(crs = 4326) %>%
    select(geometry=x) %>%
    mutate(cell_id = row_number(), x = x_cells, y = y_cells)


}


generate_group_rects <- function(){
  unit_cell <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,-1), c(0,-1), c(0,0))))
  shift_scale <- function(shift = c(0,0), scale = 1){
    (unit_cell * scale)+shift
  }
  group_rects <- st_sfc(crs = "+init=epsg:4326",
                        shift_scale(c(-126,53), c(28,29)),
                        shift_scale(c(-98, 53), c(31,13)),
                        shift_scale(c(-98, 40), c(31,16)))

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

#' borrowed from lake-surface-temperature-prep, which uses this function to match NLDAS cells
#' We'll use it here to match ERA5 cells to lakes. Same concept
feature_cell_indices <- function(cell_grid, points){

  cell_grid <- sf::st_set_crs(cell_grid, NA)
  points <- sf::st_set_crs(points, NA)

  chunk_size <- 1000
  # filter points that are outside of the grid
  within_grid <- st_within(points, cell_grid) %>% lengths() %>% {. > 0}
  points <- points[within_grid, ]
  n_points <- nrow(points)

  chunk_start <- seq(1, to = n_points, by = chunk_size)
  chunk_stop <- c(tail(chunk_start, -1L) - 1, n_points)

  out_data <- tibble(site_id = rep(NA_character_, n_points),
                     x = rep(NA_integer_, n_points),
                     y = rep(NA_integer_, n_points))
  for (chnk_i in 1:length(chunk_start)){
    out_data[chunk_start[chnk_i]:chunk_stop[chnk_i], ] <- dplyr::do(points[chunk_start[chnk_i]:chunk_stop[chnk_i], ], data.frame(match_collection = st_intersects(cell_grid, st_geometry(.), sparse = FALSE))) %>%
      mutate_all(which) %>% summarise_all(unique) %>%
      tidyr::gather(key = cent_idx, value = grid_idx) %>%
      mutate(x = cell_grid[grid_idx,]$x, y = cell_grid[grid_idx,]$y) %>%
      mutate(site_id = points[chunk_start[chnk_i]:chunk_stop[chnk_i], ]$site_id) %>% select(site_id, x, y)
  }
  return(out_data)
}
