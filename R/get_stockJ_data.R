#' Get stock price history in Japan based on Yahoo! Finance webpage
#' 
#' This function scrapes the stock price information from the Yahoo! finance webpage (https://finance.yahoo.co.jp).
#'
#' @param code Numeric value representing the stock ID number that you want to search
#'
#' @return Dataframe
#' @export
get_stockJ_data <- function(code) {
  
  if (!is.numeric(code)) stop("Input `code` must be a numeric value representing the stock ID code!")
  if (length(code) != 1) stop("Input `code` must be a numeric value representing the stock ID code!")
  
  Sys.sleep(1) # to avoid scraping overloading
  
  # Scraping stock data from Yahoo finance page
  quote.url <- sprintf("https://finance.yahoo.co.jp/quote/%d.T/history", as.integer(code))
  r <- rvest::read_html(quote.url)
  tbl <- rvest::html_table(r)[[1]]
  title <- gsub("：株価時系列 - Yahoo!ファイナンス", "", rvest::html_text(rvest::html_element(rvest::html_children(r)[1], "title")))
  
  financial.data <- tbl %>% 
    dplyr::transmute(
      stock_name = title,
      date = as.Date(`日付`, tz = "Japan", format = "%Y年%m月%d日", tz = "Japan"),
      start_price = as.numeric(gsub(",", "", `始値`)),
      max_price = as.numeric(gsub(",", "", `高値`)),
      min_price = as.numeric(gsub(",", "", `安値`)),
      final_price = as.numeric(gsub(",", "", `終値`)),
      turnover = as.numeric(gsub(",", "", `出来高`)),
      final_price_corrected = as.numeric(gsub(",", "", `調整後終値*`)),
    )
  
  print(financial.data)
  
  return(financial.data)
}