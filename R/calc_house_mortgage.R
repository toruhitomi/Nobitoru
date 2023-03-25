#' Calculate house mortgage
#'
#' @param principal Numeric value of principal
#' @param interest_rate Numeric value of annual interest rate
#' @param initial_cost Numeric value of initial cost
#' @param period_year Numeric value of period of mortgage
#' @param start_date Character value of starting date
#' @param payment_method Character value (Default: "even principal") indicating a method of payment: either "even principal" or "even total"
#' @param verbose Logical value (Default: FALSE) indicating whether you want to see verbose
#' @param plotit Logical value (Default: TRUE) indicating whether you want to plot the ggplot
#'
#' @return Data frame and ggplot2 object
#' @export
calc_house_mortgage <- function (principal, interest_rate, period_year = 10, initial_cost, start_date = "2023-04-25", payment_method = "even principal", verbose = FALSE, plotit = TRUE) {
  
  # for testing
  # NeURon::init.env()
  # principal <- 7000000
  # interest_rate <- 1.84
  # period_year <- 10
  # start_date = "2023-04-25"
  # payment_method <- "even principal"
  
  # Date
  st <- lubridate::as_date(x = as.POSIXct(start_date, tz = "Japan"))
  st_y <- lubridate::year(st)
  st_m <- lubridate::month(st)
  
  # Period
  n_months <- period_year * 12
  
  # Interest
  rr_year <- interest_rate / 100
  rr_month <- rr_year / 12
  
  # Make a data frame
  df <- tibble::tibble(
    id = seq(0, length.out = n_months + 1),
    year = c(rep(st_y, 12 - st_m + 1),
             rep((st_y + 1):(st_y + (period_year - 1)), each = 12), 
             rep(st_y + period_year, st_m)),
    month = c(st_m:12, rep(1:12, period_year - 1), 1:st_m)
  )
  
  # 
  if (payment_method == "even principal") {
    # even principal --> fixed principal payment
    
    principal_payment <- c(0, rep(ceiling(principal / n_months), principal %% n_months), 
                           rep(floor(principal / n_months), n_months - (principal %% n_months)))
    
    total_payment <- interest_payment <- unpaid_balance <- numeric(nrow(df))
    for (ii in 1:nrow(df)) {
      if (ii == 1) {
        unpaid_balance[ii] <- principal
        total_payment[ii] <- 0
      } else {
        unpaid_balance[ii] <- unpaid_balance[ii - 1] - principal_payment[ii]
        interest_payment[ii] <- round(unpaid_balance[ii] * rr_month)
        total_payment[ii] <- round(unpaid_balance[ii] * rr_month) + principal_payment[ii]
      }
    }
    
    df1 <- df %>% 
      mutate(total_payment, interest_payment, principal_payment, unpaid_balance) %>% 
      mutate(tax_deduction = ifelse(year <= (st_y + 10) & month == 12, floor(0.007 * unpaid_balance), 0))
    
    df2 <- df1 %>% 
      summarise(
        sum_interest = sum(interest_payment),
        sum_tax_deduction = sum(tax_deduction, na.rm = TRUE)
      )
    
    if (verbose) {
      cat(sprintf("\nTotal interest payment: ¥%d  Total tax deduction: ¥%d\n\nTotal profit: ¥%d", 
                  df2$sum_interest, df2$sum_tax_deduction, df2$sum_tax_deduction - df2$sum_interest - initial_cost))  
    }
    
    if (plotit) {
      gg <- df1 %>% 
        select(-unpaid_balance) %>% 
        pivot_longer(cols = ends_with("_payment")) %>% 
        mutate(name = gsub("_payment", "", name)) %>% 
        filter(id > 0) %>% 
        ggplot(aes(x = id, y = value)) +
        geom_line(aes(color = name, group = name)) +
        geom_point(aes(y = tax_deduction, color = "tax deduction"), data = df1 %>% filter(tax_deduction > 0), na.rm = T, size = 3) +
        labs(x = "Payment", y = "(¥)", color = "Payment type",
             title = sprintf("\nTotal interest payment: ¥%d  Total tax deduction: ¥%d\n\nTotal profit: ¥%d", 
                             df2$sum_interest, df2$sum_tax_deduction, df2$sum_tax_deduction - df2$sum_interest - initial_cost)) +
        scale_x_continuous(breaks = seq(0, n_months, by = 10)) +
        scale_y_continuous(labels = scales::dollar_format(prefix = "¥")) +
        theme(legend.position = "top", axis.title.y = element_text(angle = 0),
              title = ggtext::element_markdown())
      print(gg)
      
    }
    
    return(df1)
    
    
  } else if (payment_method == "even total") {
    # under construction
  } else {
    stop("Payment method must be either 'even principal' or 'even total'!")
  }
  rr_month
  
}