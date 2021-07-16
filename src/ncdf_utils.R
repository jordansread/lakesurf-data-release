

#' build a single file with all of the variables in the `nc_hash_fl` for each group_id
#'
#' @param hash_fileout the name of the hashtable file that contains info for the nc files created
#' @param nc_hash_fl the file information for each of the full daily nc files
#' @param cell_id_groups data.frame of group_ids and corresponding NLDAS indices
build_driver_nc <- function(hash_fileout, nc_hash_fl, cell_id_groups, weather_metadata, export_range, out_pattern){


  instance_dim_name <- 'weather_id'
  nc_files <-  tibble(filepath = file.path('/Volumes/ThunderBlade 1/HOLDER_TEMP_R/lake-surface-temperature-prep',
                                           names(yaml::yaml.load_file(nc_hash_fl)))) %>%
    mutate(variable = stringr::str_extract(filepath, '(?<=var\\[).+?(?=\\])'))

  files_out <- c()

  for (this_group_id in unique(cell_id_groups$group_id)){

    these_cells <- filter(cell_id_groups, group_id == this_group_id)
    group_bbox <- unique(these_cells$group_bbox)
    file_out <- sprintf(out_pattern, this_group_id, group_bbox)
    this_temp_file <- str_replace(file_out, pattern = '.nc', '_uncompressed.nc')
    stopifnot(length(file_out) == 1)
    files_out <- c(files_out, file_out)
    cell_x <- these_cells %>% pull(x)
    cell_y <- these_cells %>% pull(y)
    lats <- these_cells %>% pull(weather_lat_deg)
    lons <- these_cells %>% pull(weather_lon_deg)
    weather_ids <- these_cells %>% pull(weather_id)
    if (file.exists(this_temp_file))
      unlink(this_temp_file)

    for (variable in nc_files$variable){
      variable_metadata <- weather_metadata %>% filter(name == variable)
      # access the block defined by min/max x and min/max y and the time range needed
      nc <- nc_files %>% filter(variable == !!variable) %>%
        pull(filepath) %>% ncdf4::nc_open()


      time_units <- ncdf4::ncatt_get(nc, 'Time')$units %>% stringr::str_remove("days since ")
      times <- as.Date(time_units) + ncdf4::ncvar_get(nc, 'Time')
      # subset export range:
      st_time <- which(times == as.Date(export_range)[1])

      nc_x <- ncdf4::ncvar_get(nc, 'x')
      nc_y <- ncdf4::ncvar_get(nc, 'y')
      st_x <- which(nc_x == min(cell_x))
      st_y <- which(nc_y == min(cell_y))
      cnt_x <- range(cell_x) %>% diff %>% {. + 1}
      cnt_y <- range(cell_y) %>% diff %>% {. + 1}
      cnt_time <- as.Date(export_range) %>% diff %>% {. + 1} %>% as.numeric()
      these_data <- ncdf4::ncvar_get(nc, variable, start = c(st_x, st_y, st_time), count = c(cnt_x, cnt_y, cnt_time))
      ncdf4::nc_close(nc)
      # flatten 3D cube into 2D matrix by giving sparse x/y cell pairs
      # need to use mapply or matrix indexing for this otherwise it is length(x) * length(y) instead of just length(x)
      # used https://stackoverflow.com/questions/41384466/subset-matrix-with-arrays-in-r
      time_dim <- dim(these_data)[3]
      sub_index <- cbind(rep(cell_x - nc_x[st_x] + 1, each = time_dim),
                     rep(cell_y - nc_y[st_y] + 1, each = time_dim), 1:time_dim)
      sub_data <- matrix(these_data[sub_index], nrow = time_dim) %>% as.data.frame() %>% setNames(weather_ids)
      rm(sub_index)
      rm(these_data)

      # write this variable to the netcdf file:
      write_timeseries_dsg(this_temp_file, instance_names = weather_ids, lats = lats, lons = lons, alts = NA,
                           times = seq(as.POSIXct(export_range[1], tz = 'GMT'), by = 'days', length.out = time_dim),
                           data = sub_data,
                           data_unit = rep(variable_metadata$units, length(weather_ids)), data_prec = "double",
                           data_metadata = list(name = variable_metadata$name, long_name = variable_metadata$long_name),
                           instance_dim_name = instance_dim_name,
                           dsg_timeseries_id = instance_dim_name,
                           time_units = "days since 1970-01-01 00:00:00", attributes = list(),
                           coordvar_long_names = list(instance = "identifier for weather grid cell", time = "date of prediction",
                                                      lat = "latitude of grid cell centroid", lon = "longitude of grid cell centroid"),
                           add_to_existing = ifelse(file.exists(this_temp_file), TRUE, FALSE), overwrite = TRUE)

      rm(sub_data)
    }
    # delete the new main target file if it already exists
    if (file.exists(file_out))
      unlink(file_out)
    old_dir <- setwd(dirname(file_out))
    # --ppc key1=val1#key2=val2
    precision_args <- paste(paste(weather_metadata$name, weather_metadata$precision, sep = '='), collapse = '#')
    # compress and quantize the file
    system(sprintf("ncks -h --fl_fmt=netcdf4 --cnk_plc=g3d --cnk_dmn time,10 --ppc %s %s %s",
                   precision_args, basename(this_temp_file), basename(file_out)))
    setwd(old_dir)
    unlink(this_temp_file)
  }
  sc_indicate(hash_fileout, data_file = files_out)
}


build_prediction_nc <- function(hash_fileout, pred_dir, export_range, out_pattern, site_id_groups, predict_metadata, dummy){

  # need the expected range of time. Used to populate nc file and checked for each feather file.
  time_length <- diff(as.Date(export_range)) %>% as.numeric() + 1
  instance_dim_name <- 'site_id'
  files_out <- c()
  for (this_group_id in unique(site_id_groups$group_id)){
    # we have spatial groups, iterate through each group
    these_lakes <- filter(site_id_groups, group_id == this_group_id)
    group_bbox <- unique(these_lakes$group_bbox)
    file_out <- sprintf(out_pattern, this_group_id, group_bbox)
    stopifnot(length(file_out) == 1)

    # pull out all of the data that will be part of writing this group's file:
    files_out <- c(files_out, file_out)
    site_ids <- pull(these_lakes, site_id)
    lats <- pull(these_lakes, lake_lat_deg)
    lons <- pull(these_lakes, lake_lon_deg)
    elevations <- pull(these_lakes, elevation_m)

    # pre-populate the matrix for data
    data_out <- matrix(rep(NA_real_, time_length * length(site_ids)), ncol = length(site_ids))
    skipped_ids <- c()
    for (i in 1:length(site_ids)){
      site_id <- site_ids[i]
      # read this file in
      this_slice <- arrow::read_feather(file.path(pred_dir, sprintf('outputs_%s.feather', site_id))) %>%
        pull(temp_pred)

      if (length(this_slice) == nrow(data_out)){
        data_out[, i] <- this_slice
      } else {
        skipped_ids <- c(skipped_ids, site_id)
      }

    }
    # convert to data.frame since that is what the write_timeseries_dsg() file expects
    data_out <- as.data.frame(data_out) %>% setNames(site_ids)

    if (length(skipped_ids) > 0){
      stop(this_group_id, ' failing ', length(skipped_ids), ' because they are incomplete')
    }


    this_temp_file <- str_replace(file_out, pattern = '.nc', '_uncompressed.nc')
    if (file.exists(this_temp_file)){
      unlink(this_temp_file)
    }
    # write this variable to the netcdf file:
    write_timeseries_dsg(this_temp_file, instance_names = site_ids, lats = lats, lons = lons, alts = elevations,
                         times = seq(as.POSIXct(export_range[1], tz = 'GMT'), by = 'days', length.out = time_length),
                         data = data_out,
                         data_unit = rep(predict_metadata$units, length(site_ids)), data_prec = "double",
                         data_metadata = list(name = predict_metadata$name, long_name = predict_metadata$long_name),
                         time_units = "days since 1970-01-01 00:00:00", attributes = list(),
                         instance_dim_name = instance_dim_name,
                         dsg_timeseries_id = instance_dim_name,
                         coordvar_long_names = list(instance = "identifier for the lake location; NHDHR PermID", time = "date of prediction",
                                                    lat = "latitude of lake centroid", lon = "longitude of lake centroid",
                                                    alt = "approximate elevation of lake surface"),
                         add_to_existing = FALSE, overwrite = TRUE)

    # big matrix of data. Get rid of it since it is no longer needed. Probably unnecessary.
    rm(data_out)

    # delete the new main target file if it already exists
    if (file.exists(file_out))
      unlink(file_out)

    # better to run these ncdf commands from the directory of the files:
    old_dir <- setwd(dirname(file_out))

    # compress and quantize the file

    system(sprintf("ncks -h --fl_fmt=netcdf4 --cnk_plc=g3d --cnk_dmn time,10 --ppc %s=%s %s %s",
                   predict_metadata$name, predict_metadata$precision, basename(this_temp_file), basename(file_out)))

    setwd(old_dir)
    unlink(this_temp_file)
  }
  sc_indicate(hash_fileout, data_file = files_out)
}

