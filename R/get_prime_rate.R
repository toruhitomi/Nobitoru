
#' Get prime rate information from Bank of Japan website (https://www.boj.or.jp/statistics/dl/loan/prime/prime.htm)
#'
#' @param plotit Logical value (Default: TRUE) indicating whether you want to plot the result
#'
#' @return Data frame (and ggplot object)
#' @export
get_prime_rate <- function(plotit = TRUE) {
  
  pacman::p_load(rvest, XML, tidyverse)
  
  url <- "https://www.boj.or.jp/statistics/dl/loan/prime/prime.htm"
  Sys.sleep(1)
  html <- read_html(url)
  tbl <- html_element(html, css = "#contents > div > div > table > tbody") %>% 
    html_table()
  
  tbl0 <- tbl %>% 
    magrittr::set_colnames(c("date", "short_pr_mode", "short_pr_max", "short_pr_min", "long_pr"))
  
  # preprocessing
  tbl1 <- tbl0 %>% 
    mutate(
      year = mClean(stringr::str_split(stringr::str_split(date, "（", simplify = T)[,2], "）", simplify = T)[,1]),
      month = mClean(stringr::str_sub(date, stringr::str_locate(date, "年")[,1] + 1, stringr::str_locate(date, "月")[,1] - 1)),
      day = mClean(stringr::str_sub(date, stringr::str_locate(date, "月")[,1] + 1, stringr::str_locate(date, "日")[,1] - 1))
    ) %>% 
    mutate(date = as_date(lubridate::ymd(sprintf("%d-%d-%d", year, month, day), tz = "Japan")))
  
  tbl2 <- tbl1 %>% 
    mutate(row_id = 1:nrow(.), .before = 1) %>% 
    group_by(row_id) %>% 
    mutate_at(
      .vars = dplyr::vars(c(short_pr_mode, short_pr_max, short_pr_min, long_pr)), 
      .funs = function (x) {
        if (stringr::str_detect(x, "（")) {
          xx <- stringr::str_split(x, "（", simplify = T)[,1] 
        } else if (stringr::str_detect(x, "↓")) {
          xx <- "↓"
        } else if (stringr::str_detect(x, "不定")) {
          xx <- NA
        } else {
          xx <- x
        }
        return(xx)
      }
    )
  
  for (ii in 1:nrow(tbl2)) {
    
    if (ii > 1) {
      
      if (!is.na(tbl2$short_pr_mode[ii])) {
        if (tbl2$short_pr_mode[ii] == "↓") {
          tbl2$short_pr_mode[ii] <- tbl2$short_pr_mode[ii - 1]
        }
      }
      if (!is.na(tbl2$short_pr_max[ii])) {
        if (tbl2$short_pr_max[ii] == "↓") {
          tbl2$short_pr_max[ii] <- tbl2$short_pr_max[ii - 1]
        }
      }
      if (!is.na(tbl2$short_pr_min[ii])) {
        if (tbl2$short_pr_min[ii] == "↓") {
          tbl2$short_pr_min[ii] <- tbl2$short_pr_min[ii - 1]
        }
      }
      if (!is.na(tbl2$long_pr[ii])) {
        if (tbl2$long_pr[ii] == "↓") {
          tbl2$long_pr[ii] <- tbl2$long_pr[ii - 1]
        }
      }
      
    }
    
  }
  
  tbl3 <- tbl2 %>% 
    mutate_at(.vars = dplyr::vars(dplyr::contains("_pr")), .funs = function(x) as.numeric(x)) %>% 
    ungroup()
  
  if (plotit) {
    tbl3 %>%
      pivot_longer(short_pr_mode:long_pr) %>% 
      mutate(name = case_when(
        str_detect(name, "short") ~ sprintf("Short-term rate %s", str_split(name, "_", simplify = T)[,3]),
        TRUE ~ "Long-term rate"
      )) %>% 
      ggplot(aes(x = date, y = value, color = name)) +
      geom_line(aes(group = name)) +
      labs(x = "Date", y = "Prime rate", color = "Type") +
      hrbrthemes::theme_ipsum_tw(base_size = 13, axis_title_size = 13) +
      theme(legend.position = "top")
  }
    
  return(tbl3)
}

#' @export
mClean <- function(strVec){
  pass1 <- strVec %>% 
    stringr::str_trim() %>% 
    stringr::str_extract("(?x)        # Perl-style whitespace
                         ^[\\+\\-]?   # An optional leading +/-
                         \\d+         # the integer part
                         (\\.\\d+)? # A fractional part
                         ") %>% 
    as.numeric()
}