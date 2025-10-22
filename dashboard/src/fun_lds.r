fun_lds <- function(x) {
  y <- x %>%
    purrr::map(dplyr::bind_rows)
  
  output <- purrr::map2(
    .x = y,
    .y = names(y),
    ~.x %>% dplyr::mutate(metric = .y, .before = 1)
  )
  
  output %>%
    dplyr::bind_rows()
  
}