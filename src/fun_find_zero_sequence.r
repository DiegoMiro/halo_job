fun_find_zero_sequence <- function(df_diff) {
  
  df_sequence <- purrr::map(
    3:nrow(df_diff),
    \(z) purrr::map2(
      1:(nrow(df_diff) - z + 1),
      (z):nrow(df_diff),
      \(x, y) seq(x, y)
    )
  ) %>%
    purrr::flatten() %>%
    tibble::tibble(sequence = .) %>%
    mutate(
      sequence_flattern = sequence %>%
        purrr::map_chr(stringr::str_c, collapse = "-"),
      sequence_size = sequence %>%
        purrr::map_int(length),
      sequence_count_zero = sequence %>%
        purrr::map_dbl(
          \(x) df_diff %>%
            dplyr::slice(x) %>%
            dplyr::filter(d == 0) %>%
            nrow()
        ),
      sequence_avg_points = sequence %>%
        purrr::map_dbl(
          \(x) df_diff %>%
            dplyr::slice(x) %>%
            dplyr::pull(d) %>%
            mean()
        )
    ) %>%
    arrange(sequence_avg_points, dplyr::desc(sequence_size)) %>%
    mutate(flg = 0L) %>%
    rowid_to_column()
  
  
  for (i in rev(seq_len(nrow(df_sequence))[-1])) {
    if (
      any(
        stringr::str_detect(
          df_sequence$sequence_flattern[1:(i-1)],
          df_sequence$sequence_flattern[i]
        )
      )
    ) df_sequence$flg[i] <- 1L
    # if (
    #   all(unlist(df_sequence$sequence[i]) %in% unlist(df_sequence$sequence[i-1]))
    # ) df_sequence$flg[i] <- 1L
  }
  
  return(df_sequence)
  
}
