generate_group_rects <- function(){
  unit_cell <- st_polygon(list(rbind(c(0,0), c(1,0), c(1,-1), c(0,-1), c(0,0))))
  shift_scale <- function(shift = c(0,0), scale = 1){
    (unit_cell * scale)+shift
  }
  group_rects <- st_sfc(crs = "+init=epsg:4326",
                        shift_scale(c(-104.25,49), c(9.25,2)),
                        shift_scale(c(-95,49.5), c(3.5,1.5)),
                        shift_scale(c(-91.5,49), c(2.5,1)),
                        shift_scale(c(-95,48)),
                        shift_scale(c(-94,48), c(1, 0.5)),
                        shift_scale(c(-94,47.5), c(1, 0.5)),
                        shift_scale(c(-93,48), c(1, 2)),
                        shift_scale(c(-92,48)),
                        shift_scale(c(-91,48), c(1, 2)),
                        shift_scale(c(-90,48), c(1,2)),
                        shift_scale(c(-89,47.5), c(5.5, 1.5)),
                        shift_scale(c(-104,47), c(8, 3)), #12
                        shift_scale(c(-96,47), c(1.5,0.5)),
                        shift_scale(c(-96,46.5), c(1.5,0.5)),
                        shift_scale(c(-94.5,47), c(0.5,1)),
                        shift_scale(c(-94,47)),
                        shift_scale(c(-92,47)),
                        shift_scale(c(-92,46)),
                        shift_scale(c(-91,46), c(1.5,0.5)),
                        shift_scale(c(-96,45), c(2.5,1)),
                        shift_scale(c(-100,44), c(9,3.75)), #21
                        shift_scale(c(-91,45.5), c(1.5,7.5)),
                        shift_scale(c(-89.5,46), c(0.5,1)),
                        shift_scale(c(-89,46)),
                        shift_scale(c(-88,46), c(3,3)),
                        shift_scale(c(-85,46), c(2,3)),
                        shift_scale(c(-89.5,45), c(1.5,2)),
                        shift_scale(c(-89.5,43), c(4,1.5)),
                        shift_scale(c(-85.5,43), c(3,1.5)),
                        shift_scale(c(-89.5,41.5), c(5,4.5)), #30
                        shift_scale(c(-96,46), c(1.5,1)),
                        shift_scale(c(-94.5,46)),
                        shift_scale(c(-93.5,46)),
                        shift_scale(c(-92.5,46), c(0.5,1)),
                        shift_scale(c(-93.5,45), c(2.5,1)))

  groups <- data.frame(group_id = rep(NA_character_, length(group_rects)), stringsAsFactors = FALSE)
  for (i in 1:length(group_rects)){
    # format of N46.125-48.625_W86.5-89.25
    this_box <- st_bbox(group_rects[i])
    groups$group_id[i] <- sprintf("%02d_N%1.2f-%1.2f_W%1.2f-%1.2f", i, this_box$ymin, this_box$ymax, -this_box$xmax, -this_box$xmin)
  }

  return(st_sf(groups, group_rects))

}

assign_group_id <- function(points, polygons){
  # to test visually
  # plot(polygons, reset = F)
  # plot(st_geometry(points), add = TRUE)
  points %>% mutate(group_id = {st_intersects(x = points, y = polygons) %>% unlist %>% polygons$group_id[.]}) %>%
    st_drop_geometry()
}
