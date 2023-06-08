#' Get stock price history in Japan based on Yahoo! Finance webpage
#' 
#' This function scrapes the stock price information from the Yahoo! finance webpage (https://finance.yahoo.co.jp).
#'
#' @param code Character value representing the stock ID number/label that you want to search
#' @param time_unit Character value (Default: "d") indicating a unit of time to be displayed either 'd' (daily), 'w' (weekly), or 'm' (monthly).
#' @param date_from Numeric/Character value (Default: NULL) indicating date from which data will be extracted.
#' @param date_to Numeric/Character value (Default: NULL) indicating date to which data will be extracted.
#' @param plotit Logical value (Default: TRUE) indicating whether you want to plot the time-series or not
#'
#' @return Dataframe
#' @export
get_stockJ_data <- function(code, time_unit = "d", date_from = NULL, date_to = NULL, plotit = TRUE) {
  
  if (length(code) != 1) stop("Input `code` must be a character value representing the stock ID/ code!")
  
  if (length(time_unit) != 1) stop("Input `time_unit` must be a character either 'd' (daily), 'w' (weekly), or 'm' (monthly)!")
  if (!time_unit %in% c("d", "w", "m")) stop("Input `time_unit` must be a character either 'd' (daily), 'w' (weekly), or 'm' (monthly)!")
  
  # Scraping stock data from Yahoo finance page
  quote.url <- sprintf("https://finance.yahoo.co.jp/quote/%s/history", code)
  
  extra.url <- sprintf("?timeFrame=%s", time_unit)
  # Start date
  if (!is.null(date_from)) {
    if (is.numeric(date_from)) {
      date_from <- as.character(date_from)
    } else if (is.character(date_from)) {
      # do nothing
    } else {
      stop("Input `date_from`/`date_to` must be either numeric or character 'YYYYmmdd' format (e.g., 20230609)!")
    }
    if (nchar(date_from) != 8) stop("Input `date_from`/`date_to` must be either numeric or character 'YYYYmmdd' format (e.g., 20230609)!")
    
    extra.url <- sprintf("%s&from=%s", extra.url, date_from)
  } 
  
  # To date
  if (!is.null(date_to)) {
    if (is.numeric(date_to)) {
      date_to <- as.character(date_to)
    } else if (is.character(date_to)) {
      # do nothing
    } else {
      stop("Input `date_from`/`date_to` must be either numeric or character 'YYYYmmdd' format (e.g., 20230609)!")
    }
    if (nchar(date_to) != 8) stop("Input `date_from`/`date_to` must be either numeric or character 'YYYYmmdd' format (e.g., 20230609)!")
    
    extra.url <- sprintf("%s&to=%s", extra.url, date_to)
  } 
  
  quote.url2 <- sprintf("%s%s", quote.url, extra.url)
  
  r <- rvest::read_html(sprintf("%s&page=1", quote.url2))
  Sys.sleep(1) # to avoid scraping overloading
  
  title <- gsub("：株価時系列 - Yahoo!ファイナンス", "", rvest::html_text(rvest::html_element(rvest::html_children(r)[1], "title")))
  
  n_items <- rvest::html_children(r)[2] %>% 
    rvest::html_elements(xpath = '//*[@id="pagerbtm"]') %>% 
    # rvest::html_elements("ul > li") %>% 
    rvest::html_elements("p") %>% 
    rvest::html_text()
  n_items <- as.numeric(gsub("件", "", strsplit(n_items, "[/]")[[1]][2]))
  n_pages <- ceiling(n_items / 20)
  
  cat(sprintf("Obtaining stock price history of %s ... total %d pages", title, n_pages), sep = "\n")
  
  # Page 1
  tbl <- rvest::html_table(r)[[1]]
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
  cat(sprintf("page 1 / %d", n_pages), sep = "\n")
  
  # Page 2 ~
  for (ii in 2:n_pages) {
    
    r <- rvest::read_html(sprintf("%s&page=%d", quote.url2, ii))
    Sys.sleep(1) # to avoid scraping overloading
    
    tbl <- rvest::html_table(r)[[1]]
    financial.data <- financial.data %>% 
      dplyr::bind_rows(
        tbl %>% 
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
      )
    
    cat(sprintf("page %d / %d", ii, n_pages), sep = "\n")
  }
  
  print(financial.data)
  
  if (plotit) {
    gg_ts <- financial.data %>% 
      ggplot2::ggplot(ggplot2::aes(x = date, y = final_price)) +
      ggplot2::geom_line() +
      ggplot2::theme_bw(14) +
      ggplot2::geom_point(data = financial.data[c(1, nrow(financial.data)),], color = c("#1E90FF")) +
      ggrepel::geom_label_repel(ggplot2::aes(label = scales::dollar(final_price, prefix = "¥")), data = financial.data[c(nrow(financial.data)),], nudge_x = -10, seed = 123, min.segment.length = .01, color = c("#1E90FF")) +
      ggrepel::geom_label_repel(ggplot2::aes(label = scales::dollar(final_price, prefix = "¥")), data = financial.data[c(1),], nudge_x = 10, seed = 123, min.segment.length = .01, color = c("#1E90FF")) +
      ggplot2::scale_y_continuous(labels = scales::dollar_format(prefix = "¥")) +
      ggplot2::labs(x = "Date", y = "Final price", title = sprintf("%s (%s ~ %s)", title, financial.data$date[1], financial.data$date[nrow(financial.data)])) +
      ggplot2::scale_x_date(expand = c(.15, .15))
      
    print(gg_ts)
  }
  
  return(financial.data)
}