library(shiny)
library(shinydashboard)
library(gsheet)
library(htmlwidgets)
library(tidyverse)
library(plotly)
theme_set(theme_bw(base_size = 17))


# Read the weight data from Google sheet ----------------------------------
weight_data <- gsheet2text(url = "https://docs.google.com/spreadsheets/d/10XMGCynrMzbaxKnchAh0QmuaVW1F8FdH0Gx0zZwC7fU/edit?usp=sharing",
                           format = "csv") %>% 
    read_csv(col_names = c("Date", "Time", "Toru_weight", "Toru_%fat", "Novi_weight", "Novi_%fat", "burnt_cal", "n_steps", "distance_km"), 
             skip = 1, locale = locale(encoding = "utf8")) %>% 
    mutate(Date = as.Date(Date, tryFormats = "%m/%d")) %>% 
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
current_month <- which(month.abb == stringr::str_split(date(), " ")[[1]][2])
sheetids <- sprintf("%d%02d", init_year, seq(from = init_month, to = current_month))

# This spreadsheet has multiple sheets each of which is to be accessed
library(googledrive)
library(googlesheets4)
budget_data <- drive_get("Budget_Nobitoru_202004-")
sheet_id <- googlesheets4::as_sheets_id(budget_data) %>% 
    sheet_properties()
infra_data <- read_sheet(ss = budget_data, sheet = "Infra")
monthly <- sheetids %>% purrr::map_dfr(.x = ., .f = read_sheet, 
                                       ss = budget_data, range = "A1:H50") %>% 
    tidyr::drop_na(Month)

ba <- monthly %>% 
    dplyr::filter(Month %in% c("Before", "After")) %>% 
    dplyr::select(c("Month", dplyr::starts_with("Bank"))) %>% 
    dplyr::mutate(BeforeAfter = unlist(Month),
                  YearMonth = rep(sheetids, each = 2)) %>% 
    dplyr::select(YearMonth, BeforeAfter, Bank_Mizuho, Bank_JP)
ba

daily <- tibble::tibble()
for (ii in 1:nrow(monthly)) {
    if (any(class(monthly$Month[[ii]]) == "POSIXct")) {
        m <- format(monthly$Month[[ii]], tz = "Japan", usetz = T, format = "%Y-%m-%d")
        daily <- rbind.data.frame(daily, tibble::tibble(
                Date = m,
                monthly[ii, 2:ncol(monthly)]
            ))
    }
}

update_mizuho <- ba[[1, "Bank_Mizuho"]]
update_jp     <- ba[[1, "Bank_JP"]]
for (ii in 1:nrow(daily)) {
    update_mizuho <- update_mizuho + daily[ii, "Bank_Mizuho"]
    update_jp     <- update_jp     + daily[ii, "Bank_JP"]
    daily[ii, "Bank_Mizuho"] <- update_mizuho
    daily[ii, "Bank_JP"] <- update_jp
}
# daily

# UI section --------------------------------------------------------------
ui <- navbarPage(
    "Nobitoru App",
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
    tabPanel("Budget Record",
             div(plotlyOutput("budgetPlot", height = "90%"), style = "height: calc(100vh - 200px)")),
    tabPanel("Nobitoru", 
             div(img(src = "Nobitoru_pic.jpg", align = "center", height = 150*3, width = 200*3)),
             h3("About Nobitoru"),
             fluidRow(
                 column(12, p("Novi: lovely active but laidback woman who loves thinking about food, life styles, and buying houses."),
                            p("Toru: cool and a bit nerdy man who loves thinking about food, science and books."))
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
            geom_hline(data = avg, aes(yintercept = meanData, color = Nobitoru, group = Nobitoru), lty = "dotted", size = 1) +
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
            select(1:3) %>% 
            mutate(Total = Bank_Mizuho + Bank_JP) %>% 
            pivot_longer(cols = Bank_Mizuho:Total, names_to = "Bank", values_to = "Balance") %>% 
            ggplot(aes(x = Date, y = Balance/1000000, color = Bank, group = Bank)) +
            geom_point() +
            geom_line() + 
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90)) +
            scale_color_manual(values = c("#1BD158", "#417CFC", "red")) +
            labs(y = "Balance (M)", title = "Toru's Bank Balance")
        ggplotly(budget_track)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


