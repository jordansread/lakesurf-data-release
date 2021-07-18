



assign_group_id <- function(points, polygons, use_col){
  # to test visually
  # plot(polygons, reset = F)
  # plot(st_geometry(points), add = TRUE)
  # drop CRS so that sf treats these like rectangles instead of curved shapes for the purposes of overlap/intersection
  polygons <- sf::st_set_crs(polygons, NA)
  points <- sf::st_set_crs(points, NA)

  box_subset <- polygons %>% st_drop_geometry()
  points %>% mutate(group_id = {st_intersects(x = points, y = polygons) %>% unlist %>% polygons$group_id[.]}) %>%
    st_drop_geometry() %>%
    left_join(box_subset, by = 'group_id') %>%
    select(group_id, group_bbox, !!use_col)
}
