#' Calculate house mortgage
#'
#' @param principal Numeric value of principal
#' @param interest_rate Numeric value of annual interest rate
#' @param initial_cost Numeric value of initial cost
#' @param period_year Numeric value of period of mortgage
#' @param start_date Character value of starting date
#' @param payment_method Character value (Default: "even principal") indicating a method of payment: either "even principal" or "even total"
#' @param payoff_date Character value (Default: NULL) indicating whether or not you assume payoff earlier than planned. When you put date of repayment, this function calculates all indices and total profit at the time of repayment.
#' @param verbose Logical value (Default: FALSE) indicating whether you want to see verbose
#' @param plotit Logical value (Default: TRUE) indicating whether you want to plot the ggplot
#'
#' @return Data frame and ggplot2 object
#' @export
calc_house_mortgage <- function (principal, interest_rate, period_year = 20, initial_cost, 
                                 start_date = "2023-04-25", payment_method = "even principal", 
                                 payoff_date = NULL,
                                 verbose = FALSE, plotit = TRUE) {
  
  # for testing
  # NeURon::init.env()
  # principal <- 7000000
  # interest_rate <- 0.65
  # period_year <- 20
  # initial_cost <- 55000 + 80000+28000+10000
  # start_date = "2023-06-25"
  # payoff_date = "2033-06-25"
  # payment_method <- "even principal"
  # verbose = T
  # plotit = T
  
  # Date
  st <- lubridate::as_date(x = as.POSIXct(start_date, tz = "Japan"))
  st_y <- lubridate::year(st)
  st_m <- lubridate::month(st)
  st_d <- lubridate::day(st)
  
  # Period
  n_months <- period_year * 12
  
  # Interest
  rr_year <- interest_rate / 100
  rr_month <- rr_year / 12
  
  # Make a data frame
  df <- tibble::tibble(
    id = seq(0, length.out = n_months + 1), # include first month with no repayment (id = 0)
    year = c(rep(st_y, 12 - st_m + 1), # 1st year
             rep((st_y + 1):(st_y + (period_year - 1)), each = 12), 
             rep(st_y + period_year, st_m)), # last year
    month = c(st_m:12, rep(1:12, period_year - 1), 1:st_m)
  )
  
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
    
    if (!is.null(payoff_date)) {
      # Payoff the rest of your loan early
      po_y <- lubridate::year(payoff_date)
      po_m <- lubridate::month(payoff_date)
      
      df1 <- df1 %>% 
        dplyr::filter(id <= df1$id[df1$year == po_y & df$month == po_m])
      
      df2 <- df1 %>% 
        summarise(
          sum_interest = sum(interest_payment),
          sum_tax_deduction = sum(tax_deduction, na.rm = TRUE),
          unpaid_balance = min(unpaid_balance)
        )
      
    } else {
      # Complete payment period
      df2 <- df1 %>% 
        summarise(
          sum_interest = sum(interest_payment),
          sum_tax_deduction = sum(tax_deduction, na.rm = TRUE)
        )
    }
    
    #format method, which is necessary for formating in a data.frame   
    format.money  <- function(x, ...) {
      paste0("¥", formatC(as.numeric(x), format="f", digits=0, big.mark=","))
    }
    
    desc <- sprintf("\nInitial cost: \t\t-%s\nTotal interest payment: -%s \nTotal tax deduction: \t%s\n-----------------------------------\nTotal profit: \t\t%s", 
                    format.money(initial_cost), 
                    format.money(df2$sum_interest), 
                    format.money(df2$sum_tax_deduction), 
                    format.money(df2$sum_tax_deduction - df2$sum_interest - initial_cost))
    
    tmp <- tibble::tribble(
      ~Item, ~Value,
      "Initial cost", -1 * initial_cost,
      "Total interest payment", -1 * df2$sum_interest,
      "Total tax deduction", df2$sum_tax_deduction,
      "Total profit", df2$sum_tax_deduction - df2$sum_interest - initial_cost
    ) %>%
      dplyr::mutate(Value = format.money(Value))
    
    tmp[1, 2] <- kableExtra::cell_spec(tmp[1, 2], format = "html", color = "red")
    tmp[2, 2] <- kableExtra::cell_spec(tmp[2, 2], format = "html", color = "red")
    tmp[3, 2] <- kableExtra::cell_spec(tmp[3, 2], format = "html", color = "green")
    
    tmp %>%
      knitr::kable(escape = F) %>%
      kableExtra::row_spec(row = 4, bold = T)
    
    
    if (verbose) {
      cat(desc)  
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
        labs(x = "Payment months", y = NULL, color = "Payment type",
             title = gsub("\t", "    ", desc)) +
        scale_x_continuous(breaks = seq(0, n_months, by = 10)) +
        scale_y_continuous(labels = scales::dollar_format(prefix = "¥")) +
        hrbrthemes::theme_ipsum_tw(base_size = 13, axis_title_size = 13) +
        theme(legend.position = "top", axis.title.y = element_text(angle = 0),
              plot.title = element_text(hjust = 1))
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