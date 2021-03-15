

#' build a single file with all of the variables in the `nc_hash_fl` for each group_id
#'
#' @param hash_fileout the name of the hashtable file that contains info for the nc files created
#' @param nc_hash_fl the file information for each of the full daily nc files
#' @param cell_id_groups data.frame of group_ids and corresponding NLDAS indices
build_driver_nc <- function(hash_fileout, nc_hash_fl, cell_id_groups){

  out_pattern <- 'tmp/%s_weather_data.nc'

  nc_files <-  tibble(filepath = file.path('../lake-surface-temperature-prep',
                                           names(yaml::yaml.load_file(nc_hash_fl)))) %>%
    mutate(variable = stringr::str_extract(filepath, '(?<=var\\[).+?(?=\\])'))

  files_out <- c()

  for (this_group_id in unique(cell_id_groups$group_id)){
    file_out <- sprintf(out_pattern, this_group_id)
    files_out <- c(files_out, file_out)
    cell_ids <- filter(cell_id_groups, group_id == this_group_id) %>% pull(cell_id)
    cell_x <- stringr::str_extract(cell_ids, '(?<=x_).+?(?=_)') %>% as.numeric()
    cell_y <- stringr::str_extract(cell_ids, '(?<=y_).?(.*)') %>% as.numeric()

    # create the group netcdf file
    dim_cell <- ncdim_def( "cell_index", "", 1:length(cell_ids)) # for this group
    message("Don't hardcode time for ", this_group_id)
    dim_t <- ncdim_def( "Time", sprintf("days since %s", '2021-03-13'), 0:(15385-1), unlim=FALSE)
    var_cell <- ncvar_def("cell_name", "",  list(dim_cell))

    # initialize all variables here, plus cell index
    message('add variable units and long names etc')
    var_values <- lapply(nc_files$variable, FUN = function(x){
      ncvar_def(x, "unknown",  list(dim_cell, dim_t))
    })
    #browser()
    #var_cell <- list(ncvar_def('cell_name', "unknown",  list(dim_cell)))
    this_temp_file <- str_replace(file_out, pattern = '.nc', '_uncompressed.nc')
    group_nc_file <- nc_create(this_temp_file, var_values, force_v4 = TRUE)

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

      #var_values <- ncvar_def(variable, "unknown",  list(dim_cell, dim_t))

      ncvar_put(group_nc_file, varid = variable, vals = sub_data,
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


build_prediction_nc <- function(hash_fileout, pred_dir, site_id_groups){
  out_pattern <- 'tmp/%s_predicted_temperature.nc'


  files_out <- c()

  for (this_group_id in unique(site_id_groups$group_id)){
    file_out <- sprintf(out_pattern, this_group_id)
    files_out <- c(files_out, file_out)
    site_ids <- filter(site_id_groups, group_id == this_group_id) %>% pull(site_id)

    # instead, in the futre this should be a real continuous index...
    dim_cell <- ncdim_def( "site_index", "", 1:length(site_ids)) # for this group
    message("Don't hardcode time for ", this_group_id)
    dim_t <- ncdim_def( "Time", sprintf("days since %s", '2021-03-13'), 0:(14976-1), unlim=FALSE)
    var_cell <- ncvar_def("cell_name", "",  list(dim_cell))

    # initialize all variables here, plus cell index
    message('add variable units and long names etc')
    var_values <- ncvar_def('surface_temperature', "degrees C",  list(dim_cell, dim_t))

    this_temp_file <- str_replace(file_out, pattern = '.nc', '_uncompressed.nc')
    group_nc_file <- nc_create(this_temp_file, var_values, force_v4 = TRUE)

    # initialize 2D matrix to put in netcdf file
    data_out <- matrix(rep(NA_real_, 14976 * length(site_ids)), nrow = 14976)
    skipped_ids <- c()
    for (i in 1:length(site_ids)){
      site_id <- site_ids[i]
      # read this file in

      this_slice <- arrow::read_feather(file.path(pred_dir, sprintf('outputs_%s.feather', site_id))) %>% pull(temp_pred)
      if (length(this_slice) == nrow(data_out)){
        data_out[, i] <- this_slice
      } else {
        skipped_ids <- c(skipped_ids, site_id)
      }


    }
    message(this_group_id, ' skipping ', length(skipped_ids), ' because they are incomplete')
    # write this variable to the netcdf file:
    ncvar_put(group_nc_file, varid = 'surface_temperature', vals = data_out,
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
