
convert_feather_file <- function(fileout, feather_fl){

  arrow::read_feather(feather_fl) %>%
    write_csv(fileout)
}
