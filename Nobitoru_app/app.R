library(shiny)
library(shinydashboard)
library(gsheet)
library(htmlwidgets)
library(tidyverse)
theme_set(theme_bw(base_size = 17))

# Read the data from Google sheet
weight_data <- gsheet2text(url = "https://docs.google.com/spreadsheets/d/10XMGCynrMzbaxKnchAh0QmuaVW1F8FdH0Gx0zZwC7fU/edit?usp=sharing",
                           format = "csv") %>% 
    read_csv(col_names = c("Date", "Time", "Toru_weight", "Toru_%fat", "Novi_weight", "Novi_%fat", "burnt_cal", "n_steps", "distance_km"), skip = 1) %>% 
    mutate(Date = as.Date(Date, tryFormats = "%m/%d")) %>% 
    pivot_longer(cols = `Toru_weight`:distance_km, names_to = "DataType", values_to = "Data") %>% 
    mutate(Nobitoru = if_else(str_detect(.$DataType, "Novi"), "Novi", "Toru"))
for (ii in c("weight", "%fat")) {
    weight_data[str_detect(weight_data$DataType, ii), "DataType"] <- ii
}
weight_data <- weight_data %>% 
    mutate(DataType = factor(DataType, levels = c("weight", "%fat", "burnt_cal", "n_steps", "distance_km")))


ui <- navbarPage(
    "Nobitoru Info",
    tabPanel("Nobitoru", 
             div(img(src = "Nobitoru_pic.jpg", align = "center", height = 150*3, width = 200*3))),
    tabPanel("Weight Record",
             sliderInput(inputId = "dates_range",
                         "Dates to be plotted: ", 
                         min = min(weight_data$Date), 
                         max = max(weight_data$Date),
                         value = c(min(weight_data$Date), max(weight_data$Date)),
                         timeFormat = "%Y-%m-%d", 
                         width = "90%"),
             plotOutput("distPlot")
             
    ),
    tabPanel("Budget Record")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        # Generate line plot tracking weight changes across time
        weight_data %>% 
            dplyr::filter(!is.na(Data)) %>% 
            subset(Date >= input$dates_range[1] & Date <= input$dates_range[2]) %>% 
            ggplot(aes(x = Date, y = Data, color = Nobitoru)) +
            geom_point() + 
            geom_line() +
            facet_grid(rows = vars(DataType), scales = "free")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


