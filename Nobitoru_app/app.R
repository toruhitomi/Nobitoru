library(shiny)
library(shinydashboard)
library(gsheet)
library(htmlwidgets)
library(tidyverse)
library(plotly)
theme_set(theme_bw(base_size = 17))
options(dplyr.summarise.inform = F)

# Read the weight data from Google sheet ----------------------------------
weight_data <- gsheet2text(url = "https://docs.google.com/spreadsheets/d/10XMGCynrMzbaxKnchAh0QmuaVW1F8FdH0Gx0zZwC7fU/edit?usp=sharing",
                           format = "csv") %>% 
    read_csv(col_names = c("Date", "Time", "Toru_weight", "Toru_%fat", "Novi_weight", "Novi_%fat", "burnt_cal", "n_steps", "distance_km"), 
             skip = 1, locale = locale(tz = "Japan", encoding = "UTF-8"), col_types = cols()) %>% 
    mutate(Date = as.Date(Date, tryFormats = "%m/%d/%Y")) %>% 
    pivot_longer(cols = `Toru_weight`:distance_km, names_to = "DataType", values_to = "Data") %>% 
    mutate(Nobitoru = if_else(str_detect(.$DataType, "Novi"), "Novi", "Toru"))
for (ii in c("weight", "%fat")) {
    weight_data[str_detect(weight_data$DataType, ii), "DataType"] <- ii
}
weight_data <- weight_data %>% 
    mutate(DataType = factor(DataType, levels = c("weight", "%fat", "burnt_cal", "n_steps", "distance_km")))


# Read the budget data from Google sheet ----------------------------------
init_year <- 2020
init_month <- 4
current_year <- lubridate::year(Sys.Date())
current_month <- lubridate::month(Sys.Date())

if (init_year == current_year) {
    sheetids <- sprintf("%d%02d", init_year, seq(from = init_month, to = current_month))
} else if (init_year < current_year) {
    nYear <- current_year - init_year
    sheetids <- character()
    for (ii in seq(init_year, current_year)) {
        if (ii == init_year) {
            sheetids <- c(
                sheetids, sprintf("%d%02d", init_year, seq(from = init_month, to = 12))
            )
        } else if (ii == current_year) {
            sheetids <- c(
                sheetids, 
                sprintf("%d%02d", ii, seq(from = 1, to = current_month))
            )
        } else {
            sheetids <- c(
                sheetids, 
                sprintf("%d%02d", ii, seq(from = 1, to = 12))
            )
        }
    } 
}


# This spreadsheet has multiple sheets each of which is to be accessed
library(googledrive)
library(googlesheets4)
library(future)


drive_auth(email = "world.through.eyes@gmail.com")
# gs4_auth(scope = "https://www.googleapis.com/auth/drive")
gs4_auth(token = drive_token())
budget_data <- drive_get("Budget_Nobitoru_202004-")
sheet_id <- googlesheets4::as_sheets_id(budget_data) %>% 
    sheet_properties()
infra_data <- read_sheet(ss = budget_data, sheet = "Infra")

future::plan(multisession, workers = availableCores() - 1)
monthly <- sheetids %>% 
    furrr::future_map_dfr(.f = read_sheet, .id = "sheetid",
    # purrr::map_dfr(.f = read_sheet, 
                   ss = budget_data, range = "A1:G50") %>% 
    tidyr::drop_na(Month)

monthly <- monthly %>% 
    mutate(sheetid = as.numeric(sheetid)) %>% 
    left_join(
        tibble(
            sheetid = 1:length(sheetids),
            sheetName = sheetids
        )
    ) %>% 
    relocate(sheetName, .before = 1) %>% 
    dplyr::select(-sheetid)

ba <- monthly %>% 
    dplyr::filter(Month %in% c("Before", "After")) %>% 
    dplyr::select(c(Month, sheetName, dplyr::starts_with("Bank"))) %>% 
    dplyr::mutate(BeforeAfter = unlist(Month),
                  YearMonth = sheetName) %>% 
    dplyr::select(YearMonth, BeforeAfter, Bank_Mizuho, Bank_JP, Bank_Mizuho_saving)
ba

daily <- tibble::tibble()
for (ii in 1:nrow(monthly)) {
    if (any(class(monthly$Month[[ii]]) == "POSIXct")) {
        m <- format(monthly$Month[[ii]], tz = "Japan", usetz = T, format = "%Y-%m-%d")
        
        if (lubridate::year(m) == 2020 & lubridate::month(m) == 3) {
            daily <- rbind.data.frame(daily, tibble::tibble(
                Date = m,
                monthly[ii, 2:ncol(monthly)]
            ))
        } else {
            if (lubridate::year(m) == as.numeric(str_sub(monthly$sheetName[[ii]], 1, 4)) & lubridate::month(m) == as.numeric(str_sub(monthly$sheetName[[ii]], 5, 6))) {
                daily <- rbind.data.frame(daily, tibble::tibble(
                    Date = m,
                    monthly[ii, 2:ncol(monthly)]
                ))
            } else {
                # do nothing
            }
        }
    }
}

update_mizuho  <- ba[[1, "Bank_Mizuho"]]
update_jp      <- ba[[1, "Bank_JP"]]
update_mizuho2 <- ba[[1, "Bank_Mizuho_saving"]]
for (ii in 1:nrow(daily)) {
    update_mizuho  <- update_mizuho + daily[ii, "Bank_Mizuho"]
    update_jp      <- update_jp     + daily[ii, "Bank_JP"]
    if (is.na(daily[ii, "Bank_Mizuho_saving"])) {
        update_mizuho2 <- NA
    } else {
        if (is.na(update_mizuho2)) {
            update_mizuho2 <- daily[ii, "Bank_Mizuho_saving"]
        } else {
            update_mizuho2 <- update_mizuho2 + daily[ii, "Bank_Mizuho_saving"]
        }
    }
    
    daily[ii, "Bank_Mizuho"] <- update_mizuho
    daily[ii, "Bank_JP"] <- update_jp
    daily[ii, "Bank_Mizuho_saving"] <- update_mizuho2
}
# daily

# Read the ETC data from downloaded csv file ####
# "~/Google Drive/My Drive/Nobitoru/"
# drive_get(path = "Nobitoru/Finance_management/ETC_history_csv")
etc.dir <- "~/Google Drive/My Drive/Nobitoru/Finance_management/ETC_history_csv"
etc.files <- fs::dir_ls(path = etc.dir, type = "file", regexp = ".csv$$")
if (length(etc.files) == 0) {
    # do nothing
} else if (length(etc.files) > 0) {
    etc.files <- sort(etc.files)[length(etc.files)]
    etc.df <- read.csv(etc.files, header = F, skip = 1, fileEncoding = "cp932") %>% 
        tibble::as_tibble() %>% 
        dplyr::select(V3, V4, V5, V6, V11) %>% 
        magrittr::set_colnames(c("date", "time", "in_name" , "out_name", "price"))
}
# etc.df
    



# UI section --------------------------------------------------------------
ui <- navbarPage(
    "Nobitoru App",
    # * UI 1: Weight ####
    tabPanel("Weight Record", 
             sliderInput(inputId = "dates_range",
                         "Dates to be plotted: ", 
                         min = min(weight_data$Date), 
                         max = max(weight_data$Date),
                         value = c(min(weight_data$Date), max(weight_data$Date)),
                         timeFormat = "%Y-%m-%d", 
                         width = "90%"),
             radioButtons(inputId = "who",
                          label = "Whose data:", 
                          choices = c("Both", "Novi", "Toru"), 
                          selected = "Both", inline = T 
                          ),
             div(plotOutput("weightPlot", height = "90%"), style = "height: calc(100vh - 200px)")
             
    ),
    # * UI 2: Budget ####
    tabPanel("Budget Record",
             div(plotlyOutput("budgetPlot", height = "90%"), style = "height: calc(100vh - 200px)"),
             div(plotlyOutput("creditPlot", height = "90%"), style = "height: calc(100vh - 200px)")),
    
    # * UI 3: Nobitoru intro ####
    tabPanel("Nobitoru", 
             div(img(src = "Nobitoru_pic.jpg", align = "center", height = 150*3, width = 200*3)),
             h3("About Nobitoru"),
             fluidRow(
                 column(12, p("Novi: lovely active but laidback person who loves thinking about food, life styles, and buying houses."),
                            p("Toru: cool and a bit nerdy laidback person who loves thinking about food, science and books."))
             ),
             fluidRow(
                 column(12, p(
                     "History:
                              2021.10 - 2022.02 Living in Fujimi Town, Nagano, supported by Nagano Pref. (Otameshi Nagano Program)
                              2023.05 -         Purchased a house (¥8M) and started living in Fujimi Town, Nagano"
                 ))
             ))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$weightPlot <- renderPlot({
        # Generate line plot tracking weight changes across time
        weight_data <- weight_data %>% 
            dplyr::filter(!is.na(Data)) %>% 
            subset(Date >= input$dates_range[1] & Date <= input$dates_range[2])
        
        # Select whose data to be shown
        if (input$who == "Both") {
            weight_data
        } else {
            weight_data <- weight_data %>% 
                dplyr::filter(Nobitoru == input$who)
        }
        
        # Calculate average per DataType and Nobitoru
        avg <- weight_data %>% 
            group_by(DataType, Nobitoru) %>% 
            summarise(meanData = mean(Data, na.rm = T)) %>% 
            ungroup()
        
        # Generate line plot tracking weight changes across time
        g <- weight_data %>%
            ggplot(aes(x = Date, y = Data, color = Nobitoru, group = Nobitoru)) +
            geom_point() +
            geom_line() +
            geom_hline(data = avg, aes(yintercept = meanData, color = Nobitoru, group = Nobitoru), lty = "dotted", linewidth = 1) +
            facet_grid(rows = vars(DataType), scales = "free")
        
        if (input$who == "Both") {
            g <- g + scale_color_manual(values = c("#FABC5F", "#417CFC"))
        } else if (input$who == "Novi") {
            g <- g + scale_color_manual(values = "#FABC5F")
        } else {
            g <- g + scale_color_manual(values = "#417CFC")
        }
        
        print(g)
    })
    
    output$budgetPlot <- renderPlotly({
        budget_track <- daily %>% 
            dplyr::select(1, starts_with("Bank_")) %>% 
            group_by(Date) %>% 
            mutate(Total = sum(Bank_Mizuho, Bank_JP, Bank_Mizuho_saving, na.rm = T)) %>% 
            ungroup() %>% 
            pivot_longer(cols = Bank_Mizuho:Total, names_to = "Bank", values_to = "Balance") %>% 
            ggplot(aes(x = Date, y = Balance/1000000, color = Bank, group = Bank)) +
            geom_point(na.rm = T) +
            geom_line(na.rm = T) + 
            geom_smooth(method = "lm", formula = y ~ x, na.rm = T) +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90),
                  legend.position = "top") +
            scale_color_manual(values = c("#1BD158", "#417CFC", "#00CDCD", "red")) +
            labs(y = "Balance (M JPY)", title = "Nobitoru's Bank Balance in Japan",
                 caption = "Wealths outside Japan are not considered.")
        ggplotly(budget_track)
    })
    
    output$creditPlot <- renderPlotly({
        credit_track <- monthly %>% filter(str_detect(Mizuho_category, "credit payment")) %>% 
            mutate(Month = as.Date(as.POSIXct.numeric(unlist(Month), format = "%Y-%m-%d", tz = "Japan", origin = "1970-01-01"))) %>% 
            mutate(meanCredit = -1 * mean(Bank_Mizuho)) %>% 
            ggplot(aes(x = Month, y = -1 * Bank_Mizuho)) +
            geom_bar(stat = "identity", fill = c("#00B2EE"), alpha = .6) +
            geom_hline(aes(yintercept = meanCredit), color = "blue", lty = "dashed", linewidth = 1.5) +
            geom_line(aes(group = 1)) +
            geom_point(size = 1.2) +
            labs(x = NULL, y = NULL, title = "Credit payment") +
            scale_x_date(date_breaks = "1 month") +
            scale_y_continuous(labels = scales::dollar_format(prefix = "￥")) +
            coord_flip() +
            theme_bw(base_family = "Hiragino Kaku Gothic Pro W6")
        ggplotly(credit_track)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


