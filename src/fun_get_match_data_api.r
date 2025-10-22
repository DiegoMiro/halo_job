fun_get_match_data_api <- function(match_id) {
  
  # 2145647
  
  u <- glue::glue("https://fibalivestats.dcd.shared.geniussports.com/data/{match_id}/data.json")
  
  cat("Try to get data")
  cat("\n")
  cat(u)
  
  u %>%
    httr::GET() %>%
    httr::content("text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()
}