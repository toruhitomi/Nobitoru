# 0. Preparation -------------------------------------------------------------

library(shiny)
library(shinyMobile)
library(shinyWidgets)
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
#'-------------------------------------------------------------------------


# 1. UI section --------------------------------------------------------------
ui <- f7Page(
  title = "Nobitoru Mobile",
  init = f7Init(skin = "md", theme = "light"),
  f7TabLayout(
    tags$head(
      tags$script(
        "$(function(){
          $('#tappHold').on('taphold', function () {
            app.dialog.alert('Tap hold fired!');
          });
        });"
      )
    ),
    panels = tagList(
      f7Panel(side = "left", theme = "light", effect = "cover", inputId = "p1", resizable = T)
    ),
    navbar = f7Navbar(
      title = "Tabs",
      hairline = F,
      shadow = T,
      left_panel = T,
      right_panel = F
    ),
    f7Tabs(
      animated = F,
      swipeable = T,
      id = "tabs",
      f7Tab(
        tabName = "Tab 1",
        icon = f7Icon("gauge"),
        active = T,
        f7Shadow(
          intensity = 10,
          hover = T,
          f7Card(
            title = "Weight record",
            f7DatePicker(
              inputId = "dates_range1",
              label = "From",
              minDate = min(weight_data$Date), 
              maxDate = max(weight_data$Date),
              value = min(weight_data$Date),
              multiple = F,
              dateFormat = "yyyy-mm-dd",
              header = T,
              headerPlaceholder = "Start date", 
            ),
            f7DatePicker(
              inputId = "dates_range2",
              label = "Until",
              minDate = min(weight_data$Date), 
              maxDate = max(weight_data$Date),
              value = max(weight_data$Date),
              dateFormat = "yyyy-mm-dd"
            ),
            f7Radio(
              inputId = "who",
              label = "Choose whose data:",
              choices = c("Both", "Novi", "Toru"),
              selected = "Both"
            ),
            div(plotOutput("weightPlot", height = "90%"), style = "height: calc(100vh - 200px)")
          )
        )
      ),
      f7Tab(
        tabName = "Tab 2",
        icon = f7Icon("money_yen_circle"),
        active = F
      ),
      f7Tab(
        tabName = "Tab 3",
        icon = f7Icon("poultry_leg"),
        active = F
      )
    )
  )
)
#'-------------------------------------------------------------------------

# 2. Server section ----------------------------------------------------------

server <- function(input, output) {
  
  # Weight record plot
  output$weightPlot <- renderPlot({
    if (input$dates_range1[1] > input$dates_range2[1]) {
      dates_range <- c(input$dates_range2[1], input$dates_range1[1])
    } else if (input$dates_range1[1] < input$dates_range2[1]) {
      dates_range <- c(input$dates_range1[1], input$dates_range2[1])
    }
    dates_range <- as.POSIXct.Date(dates_range)
    
    weight_data <- weight_data %>%
      dplyr::filter(!is.na(Data)) %>%
      subset(Date >= dates_range[1] & Date <= dates_range[2])
    
    # Select whose data to be shown
    if (input$who == "Both") {
      weight_data
    } else {
      weight_data <- weight_data %>% 
        dplyr::filter(Nobitoru == input$who)
    }
    
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
#'-------------------------------------------------------------------------


shinyApp(ui = ui, server = server)
