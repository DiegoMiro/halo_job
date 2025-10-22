fun_parse_match_data <- function(match_data, dir_write_csv) {
  
  # dir_write_csv = "data/clean"
  
  
  
  # ---- parse date ---- #
  
  # tm ----
  ## not list column (general stats) ----
  df_general_stats <- bind_rows(
    purrr::discard(match_data$tm$`1`, is_list),
    purrr::discard(match_data$tm$`2`, is_list)
  )
  
  
  
  ## list column ----
  tm_list_vars <- match_data$tm$`1` %>%
    purrr::keep(is_list) %>%
    names()
  
  ##############################################################################
  ### logoT and logoS: this variables will be ignored because
  ###                  the links need authentication
  ##############################################################################
  ### coachDetails, assistcoach1Details, assistcoach2Details:
  ### this variables will be ignored because **I think** is no so important
  ##############################################################################
  
  ### pl ----
  df_pl <- dplyr::bind_rows(
    match_data$tm$`1`$pl %>% 
      purrr::map(purrr::list_flatten) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(tno = 1L),
    match_data$tm$`2`$pl %>% 
      purrr::map(purrr::list_flatten) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(tno = 2L)
  )
  
  
  ### shot ----
  df_shot <- dplyr::bind_rows(
    tibble::as_tibble(match_data$tm$`1`$shot),
    tibble::as_tibble(match_data$tm$`2`$shot),
  )
  
  
  ### scoring ----
  df_scoring <- dplyr::bind_rows(
    tibble::as_tibble(match_data$tm$`1`$scoring),
    tibble::as_tibble(match_data$tm$`2`$scoring)
  )
  
  
  ### lds ----
  df_lds <- dplyr::bind_rows(
    fun_lds(match_data$tm$`1`$lds),
    fun_lds(match_data$tm$`2`$lds)
  )
  
  
  
  # pbp ----
  df_pbp <- match_data$pbp %>%
    tibble::as_tibble()
  
  
  
  # leaddata ----
  ## I didnt understand well this data
  ## It's important to decide what to do with this, if this data is important
  leaddata <- match_data$leaddata
  
  
  
  # scorers ----
  df_scorers <- dplyr::bind_rows(
    tibble::as_tibble(match_data$scorers$`1`),
    tibble::as_tibble(match_data$scorers$`2`)
  ) %>%
    dplyr::select(tno, pno, player, shirtNumber, times, summary) %>%
    tibble::rowid_to_column() %>%
    tidyr::unnest(cols = times)
  
  
  # totallds ----
  df_totallds <- purrr::map2_df(
    match_data$totallds,
    names(match_data$totallds),
    \(x, y) x %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(lds = y, .before = 1)
  )
  
  
  
  # official ----
  df_official <- purrr::map2_df(
    match_data$officials,
    names(match_data$officials),
    \(x, y) x %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(official = y, .before = 1)
  )
  
  
  
  # all datasets
  output <- list(
    "general_stats" = df_general_stats,
    "pl" = df_pl,
    "shot" = df_shot,
    "scoring" = df_scoring,
    "lds" = df_lds,
    "pbp" = df_pbp,
    "scorers" = df_scorers,
    "totallds" = df_totallds,
    "official" = df_official
  ) %>%
    # some columns are only NA
    purrr::map(
      ~.x %>%
        dplyr::select(tidyselect::where(~sum(!is.na(.)) > 0))
    )
  
  
  
  # write files
  if (!missing(dir_write_csv)) {
    
    # fs::dir_create("data/clean")
    
    paths <- file.path(
      dir_write_csv,
      stringr::str_c(names(output), ".csv")
    )
    
    purrr::walk2(
      output, 
      paths,
      \(df, name) readr::write_csv(df, name)
    )
    
  }
  
  return(output)
  
}
