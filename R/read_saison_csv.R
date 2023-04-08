#' Load SAISON credit card history data from csv
#'
#' @return Data frame
#' @export
read_saison_csv <- function() {
  
  csvfiles <- list.files("~/Library/CloudStorage/GoogleDrive-world.through.eyes@gmail.com/.shortcut-targets-by-id/1K7uaXjEXhWTxdcVz23sgXM3EXyY41xFu/Nobitoru/Finance_management/Toru_credit_history_csv", 
                         full.names = T)
  
  future::plan("multisession", workers = future::availableCores() - 1)
  
  out <- csvfiles %>% 
    furrr::future_map_dfr(
      .f = function(x) {
        readr::read_csv(x, skip = 5, locale = readr::locale(encoding = "cp932"), 
                        col_names = F, col_types = readr::cols()) %>% 
          dplyr::select(-c(X3, X5)) %>% 
          magrittr::set_colnames(c("date", "requester", "payment_method", "price_jpy", "notes")) %>% 
          tidyr::drop_na(requester)
      }
    )
  
  return(out)
}
