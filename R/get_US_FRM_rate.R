#' Get U.S. mortgage rate information from Freddie Mac website
#' 
#' The mortgage rate data is taken from Federal Home Loan Mortgage Corporation (FHLMC), commonly known as Freddie Mac.
#'
#' @param plotit Logical value (Default: TRUE) indicating whether or not you want to plot the rate time-series.
#' @param st_date Character value (Default: NULL) indicating a starting date
#' @param ed_date Character value (Default: NULL) indicating a ending date
#'
#' @return Data frame (and ggplot object)
#' @export
get_US_FRM_rate <- function(plotit = TRUE, st_date = NULL, ed_date = NULL) {
  library(ggplot2)
  
  # Read data from Freddie Mac 
  df_us_rate <- openxlsx::read.xlsx(xlsxFile = "https://www.freddiemac.com/pmms/docs/historicalweeklydata.xlsx", 
                                    startRow = 8, colNames = F, detectDates = T) %>% 
    dplyr::select(X1, X2, X4) %>% 
    tibble::as_tibble() %>% 
    magrittr::set_colnames(c("week", "30yr_FRM", "15yr_FRM")) %>% 
    dplyr::filter(!stringr::str_detect(week, "Freddie Mac")) %>% 
    dplyr::mutate(week = lubridate::as_date(week))
  
  
  if (!is.null(st_date)) {
    if (is.character(st_date)) {
      st_date <- lubridate::as_date(st_date)
    } else if (lubridate::is.Date(st_date)) {
      st_date
    } else {
      stop("`st_data/ed_date` must be a character value of dates!!")
    }
    
    if (st_date < min(df_us_rate$week)) st_date <- min(df_us_rate$week)
    df_us_rate <- df_us_rate %>% 
      dplyr::filter(week >= st_date)
  }
  
  if (!is.null(ed_date)) {
    if (is.character(ed_date)) {
      ed_date <- lubridate::as_date(ed_date)
    } else if (lubridate::is.Date(ed_date)) {
      ed_date
    } else {
      stop("`st_data/ed_date` must be a character value of dates!!")
    }
    
    if (ed_date > max(df_us_rate$week)) ed_date <- max(df_us_rate$week)
    df_us_rate <- df_us_rate %>% 
      dplyr::filter(week <= ed_date)
  }
  
  # Visualization
  if (plotit) {
    # set color palette
    my_cols <- c("#43CD80", "#63B8FF")
    names(my_cols) <- c("15yr_FRM", "30yr_FRM")
    
    tmp <- df_us_rate %>% 
      tidyr::pivot_longer(dplyr::ends_with("_FRM"))
    
    gg <- tmp %>% 
      ggplot(aes(x = week, y = value)) +
      geom_vline(xintercept = tmp$week[nrow(tmp)]) +
      geom_line(aes(group = name, color = name), na.rm = TRUE) +
      geom_label(aes(x = week + 30*3, label = sprintf("%s:\n%.2f%%", week, value), group = name), 
                 hjust = 0, data = tmp[(nrow(tmp)-1):nrow(tmp),]) +
      labs(x = "Date", y = "FRM (Weekly average)", color = "Period") +
      scale_color_manual(values = my_cols) +
      scale_x_date(expand = expansion(mult = c(0, .1), add = c(0, 0))) +
      theme_bw(15, "Verdana") +
      theme(legend.position = "top")
    
    print(gg)
  }
  
  # Output
  return(df_us_rate)
  
}
