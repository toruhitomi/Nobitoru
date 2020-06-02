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
    tabPanel("Budget Record"),
    tabPanel("Nobitoru", 
             div(img(src = "Nobitoru_pic.jpg", align = "center", height = 150*3, width = 200*3)))
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
}

# Run the application 
shinyApp(ui = ui, server = server)


