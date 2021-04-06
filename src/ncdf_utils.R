

#' build a single file with all of the variables in the `nc_hash_fl` for each group_id
#'
#' @param hash_fileout the name of the hashtable file that contains info for the nc files created
#' @param nc_hash_fl the file information for each of the full daily nc files
#' @param cell_id_groups data.frame of group_ids and corresponding NLDAS indices
build_driver_nc <- function(hash_fileout, nc_hash_fl, cell_id_groups, weather_metadata){

  out_pattern <- 'tmp/%s_weather_%s.nc4'

  nc_files <-  tibble(filepath = file.path('../lake-surface-temperature-prep',
                                           names(yaml::yaml.load_file(nc_hash_fl)))) %>%
    mutate(variable = stringr::str_extract(filepath, '(?<=var\\[).+?(?=\\])'))

  files_out <- c()



  for (this_group_id in unique(cell_id_groups$group_id)){

    these_cells <- filter(cell_id_groups, group_id == this_group_id)
    group_bbox <- unique(these_cells$group_bbox)
    file_out <- sprintf(out_pattern, this_group_id, group_bbox)
    stopifnot(length(file_out) == 1)
    files_out <- c(files_out, file_out)

    cell_x <- these_cells %>% pull(x)
    cell_y <- these_cells %>% pull(y)
    lat <- these_cells %>% pull(weather_lat_deg)
    lon <- these_cells %>% pull(weather_lon_deg)
    weather_id <- these_cells %>% pull(weather_id)

    # create the group netcdf file
    dim_cell <- ncdim_def( "weather_id", "", weather_id) # for this group
    message("Don't hardcode time for ", this_group_id)
    dim_t <- ncdim_def( "Time", sprintf("days since %s", '1979-01-01'), 0:(15385-1), unlim=FALSE)
    var_lat_lon <- list(
      ncvar_def("latitude", "degrees_north",  list(dim_cell)),
      ncvar_def("longitude", "degrees_east",  list(dim_cell)))

    # initialize all variables here, plus cell index
    var_values <- lapply(nc_files$variable, FUN = function(x){
      these_metadata <- filter(weather_metadata, name == x)
      long_name <- pull(these_metadata, long_name)
      units <- pull(these_metadata, units)
      ncvar_def(x, units = units, longname = long_name, list(dim_cell, dim_t))
    })

    this_temp_file <- str_replace(file_out, pattern = '.nc', '_uncompressed.nc')

    group_nc_file <- nc_create(this_temp_file, append(var_values, var_lat_lon), force_v4 = TRUE)

    ncvar_put(group_nc_file, varid = 'latitude', vals = lat,
              start= 1, count = -1, verbose=FALSE)
    ncvar_put(group_nc_file, varid = 'longitude', vals = lon,
              start= 1, count = -1, verbose=FALSE)

    for (variable in nc_files$variable){

      # access the block defined by min/max x and min/max y and the time range needed
      nc <- nc_files %>% filter(variable == !!variable) %>%
        pull(filepath) %>% nc_open()

      st_time <- 1 # needs to be proper filtered!!!
      message('need to update time subsetting code to only export 1980-2020')
      nc_x <- ncdf4::ncvar_get(nc, 'x')
      nc_y <- ncdf4::ncvar_get(nc, 'y')
      st_x <- which(nc_x == min(cell_x))
      st_y <- which(nc_y == min(cell_y))
      cnt_x <- range(cell_x) %>% diff %>% {. + 1}
      cnt_y <- range(cell_y) %>% diff %>% {. + 1}
      these_data <- ncvar_get(nc, variable, start = c(st_x, st_y, st_time), count = c(cnt_x, cnt_y, -1))
      nc_close(nc)
      # flatten 3D cube into 2D matrix by giving sparse x/y cell pairs
      # need to use mapply or matrix indexing for this otherwise it is length(x) * length(y) instead of just length(x)
      # used https://stackoverflow.com/questions/41384466/subset-matrix-with-arrays-in-r
      time_dim <- dim(these_data)[3]
      sub_index <- cbind(rep(cell_x - nc_x[st_x] + 1, each = time_dim),
                     rep(cell_y - nc_y[st_y] + 1, each = time_dim), 1:time_dim)
      sub_data <- matrix(these_data[sub_index], nrow = time_dim)
      rm(sub_index)
      rm(these_data)

      # write this variable to the netcdf file:
      ncvar_put(group_nc_file, varid = variable, vals = t(sub_data),
                start= c(1, 1), count = c(-1,  -1), verbose=FALSE)
      rm(sub_data)
    }
    nc_close(group_nc_file)
    # delete the new main target file if it already exists
    if (file.exists(file_out))
      unlink(file_out)
    old_dir <- setwd(dirname(file_out))
    # compress the file
    system(sprintf("ncks -4 --cnk_plc=nco --cnk_map='rew' -L 1 %s %s",
                   basename(this_temp_file), basename(file_out)))
    setwd(old_dir)
    unlink(this_temp_file)
  }
  sc_indicate(hash_fileout, data_file = files_out)
}


build_prediction_nc <- function(hash_fileout, pred_dir, site_id_groups, predict_metadata, dummy){
  out_pattern <- 'tmp/%s_predicted_temp_%s.nc4'


  files_out <- c()

  for (this_group_id in unique(site_id_groups$group_id)){
    these_lakes <- filter(site_id_groups, group_id == this_group_id)
    group_bbox <- unique(these_lakes$group_bbox)
    file_out <- sprintf(out_pattern, this_group_id, group_bbox)
    stopifnot(length(file_out) == 1)

    files_out <- c(files_out, file_out)
    site_ids <- pull(these_lakes, site_id)
    predict_ids <- pull(these_lakes, predict_id)

    # instead, in the futre this should be a real continuous index...
    dim_cell <- ncdim_def( "predict_id", "", predict_ids) # for this group
    message("Don't hardcode time for ", this_group_id)
    dim_t <- ncdim_def( "Time", sprintf("days since %s", '1980-01-01'), 0:(14976-1), unlim=FALSE)

    # initialize all variables here, plus cell index
    var_values <- ncvar_def(name = predict_metadata$name,
                            units = predict_metadata$units,
                            longname = predict_metadata$long_name,  list(dim_cell, dim_t))

    this_temp_file <- str_replace(file_out, pattern = '.nc', '_uncompressed.nc')
    if (file.exists(this_temp_file)){
      unlink(this_temp_file)
    }
    group_nc_file <- nc_create(this_temp_file, var_values, force_v4 = TRUE)

    # initialize 2D matrix to put in netcdf file
    data_out <- matrix(rep(NA_real_, 14976 * length(site_ids)), nrow = length(site_ids))
    skipped_ids <- c()
    for (i in 1:length(site_ids)){
      site_id <- site_ids[i]
      # read this file in

      this_slice <- arrow::read_feather(file.path(pred_dir, sprintf('outputs_%s.feather', site_id))) %>%
        pull(temp_pred) %>% round(digits = predict_metadata$round_digits)

      if (length(this_slice) == ncol(data_out)){
        data_out[i, ] <- this_slice
      } else {
        skipped_ids <- c(skipped_ids, site_id)
      }


    }

    message(this_group_id, ' skipping ', length(skipped_ids), ' because they are incomplete')
    # write this variable to the netcdf file:
    ncvar_put(group_nc_file, varid = predict_metadata$name, vals = data_out,
              start= c(1, 1), count = c(-1,  -1), verbose=FALSE)
    rm(data_out)
    nc_close(group_nc_file)
    # delete the new main target file if it already exists
    if (file.exists(file_out))
      unlink(file_out)
    old_dir <- setwd(dirname(file_out))
    # compress the file
    system(sprintf("ncks -4 --cnk_plc=nco --cnk_map='rew' -L 1 %s %s",
                   basename(this_temp_file), basename(file_out)))
    setwd(old_dir)
    unlink(this_temp_file)
  }
  sc_indicate(hash_fileout, data_file = files_out)
}
