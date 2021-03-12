



assign_group_id <- function(points, polygons, use_col){
  # to test visually
  # plot(polygons, reset = F)
  # plot(st_geometry(points), add = TRUE)

  points %>% mutate(group_id = {st_intersects(x = points, y = polygons) %>% unlist %>% polygons$group_id[.]}) %>%
    st_drop_geometry() %>%
    select(group_id, !!use_col)
}
