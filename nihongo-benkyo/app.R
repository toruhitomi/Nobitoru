library(shiny)
library(shinyjs)
library(tidyverse)
source("hiragana.R")
source("katakana.R")
source("kana_sound.R")
source("number_reading.R")
kanji_n4 <- read.csv("N4_Kanji.csv")
# kanji_n5 <- read.csv("N5_Kanji.csv")
kanji_n5 <- readxl::read_excel("N5_Kanji.xlsx", sheet = 2)
# kanji_n5 <- kanji_n5 %>%
#     select(-1)
# colnames(kanji_n5)[4] <- "Kanji.Meaning"
# ii <- 1
# df <- tibble()
# for (ii in 1:nrow(kanji_n5)) {
#     if (!is.na(kanji_n5$Kanji[ii])) {
#         kanji <- kanji_n5$Kanji[ii]
#         onyomi_roman <- kanji_n5$Onyomi[ii]
#         kunyomi_roman <- kanji_n5$Kunyomi[ii]
#         meaning <- kanji_n5$Kanji.Meaning[ii]
# 
#     } else {
#         if (!all(is.na(as.character(kanji_n5[ii,])))) {
#             onyomi_kana <- kanji_n5$Onyomi[ii]
#             kunyomi_kana <- kanji_n5$Kunyomi[ii]
# 
#             df <- df %>%
#                 bind_rows(
#                     tibble(
#                         kanji, on.yomi = onyomi_kana, kun.yomi = kunyomi_kana, meaning
#                     )
#                 )
# 
#         }
#     }
# }
# data.table::fwrite(df, "N5_Kanji.csv", bom = T)

# Define UI for application that draws a histogram
ui <- fluidPage(
    shinyjs::useShinyjs(),
    # Application title
    titlePanel("Nihongo-Benkyo App by Toru"),

    tabsetPanel(
        tabPanel("Kana & Numbers",
            # Sidebar with a slider input for number of bins 
            sidebarLayout(
                sidebarPanel(
                    radioButtons(inputId = "kana_type",
                                 "Choose:",
                                 choices = c("ひらがな (Hiragana)", "カタカナ (Katakana)", "すうじ（Numbers）")),
                    actionButton("refresh", "Refresh")
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                    textOutput("kana"),
                    h5("Target letter"),
                    uiOutput("q_text2"),
                    textInput("ans_text", "Type the sound of the displayed letter:"),
                    shiny::actionButton("check", "Answer"),
                    div(
                        id = "feedback_div",
                        column(8, htmlOutput("feedback"))
                    )
                    
                )
            )
        ),
        tabPanel("Kanji",
                 # Sidebar with a slider input for number of bins 
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons(inputId = "kanji_level",
                                      "Choose:",
                                      choices = c("JLPT N5")),
                         actionButton("refresh_kanji", "Refresh")
                     ),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         h5("Question"),
                         verbatimTextOutput("kanji_q_text"),
                         tags$head(tags$style(paste0("#kanji_q_text{color:black; font-size:40px; text-align:center;
                                max-height: 100px;}"))),
                         div(id = "kanji_div",
                             uiOutput("kanji_choices"),
                             div(id = "check_kanji_div",
                                 shiny::actionButton("check_kanji", "Answer")),
                             column(8, htmlOutput("feedback_kanji"))
                         )
                     )
                 )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$test <- renderText({
        input$kana_type
    })
    
    kana_type <- reactive({ input$kana_type })
    refresh <- reactive({ input$refresh })
    answer <- reactive({ input$ans_text })
    kanji_level <- reactive({ input$kanji_level })
    refresh_kanji <- reactive({ input$refresh_kanji })
    check_kanji <- reactive({ input$check_kanji })
    
    # Kana/Number tab
    observeEvent(list(kana_type(), refresh()), {
        shinyjs::reset(id = "ans_text")
        shinyjs::reset(id = "feedback")
        shinyjs::reset(id = "feedback_div")
        
        # Set target kana
        if (kana_type() == "ひらがな (Hiragana)") {
            qq <- sample(hiragana(), 1)
            output$q_text <- renderText({ qq })
            q_text <- qq
        } else if (kana_type() == "カタカナ (Katakana)") {
            qq <- sample(katakana(), 1)
            output$q_text <- renderText({ qq })
            q_text <- qq
        } else if (kana_type() == "すうじ（Numbers）") {
            qq <- as.numeric(sample(1:10000, 1))
            output$q_text <- renderText({ number_reading(qq) })
            q_text <- number_reading(qq)
        }
        
        output$q_text2 <- renderUI({
            column(
                12,
                verbatimTextOutput("q_text"),
                tags$head(tags$style(paste0("#q_text{color:black; font-size:", ifelse(kana_type() == "すうじ（Numbers）", 25, 50), "px; text-align:center;
                                max-height: 100px;}")))
            )
        })
        
        # Reactively check answer and give feedback
        observeEvent(list(refresh(), input$check), {
            shinyjs::reset(id = "ans_text")
            shinyjs::reset(id = "feedback")
            shinyjs::reset(id = "feedback_div")
            
            if (answer() != "") {
                if (kana_type() %in% c("ひらがな (Hiragana)", "カタカナ (Katakana)")) {
                    idx <- NULL
                    if (kana_type() == "ひらがな (Hiragana)") {
                        idx <- which(hiragana() == qq)
                    } else if (kana_type() == "カタカナ (Katakana)") {
                        idx <- which(katakana() == qq)
                    }
                    judge <- ifelse(stringr::str_detect(tolower(answer()), kana_sound[idx]), "Correct!", "Incorrect")
                    # judge <- paste0(qq, ": ", answer(), " vs. ", kana_sound[idx])
                    output$feedback <- renderText({ 
                        paste0("<span style='color:", ifelse(judge == "Correct!", "green", "red"), ";font-size:20px'>", judge, "</span><br>   ", 
                               ifelse(judge == "Correct!", "", sprintf("Correct answer: '%s'", kana_sound[idx])))
                    })
                } else if (kana_type() == "すうじ（Numbers）") {
                    if (!is.na(suppressWarnings(as.numeric(answer())))) {
                        judge <- ifelse(as.numeric(answer()) == qq, "Correct!", "Incorrect")
                        output$feedback <- renderText({ 
                            paste0("<span style='color:", ifelse(judge == "Correct!", "green", "red"), ";font-size:20px'>", judge, "</span><br>   ",
                                   ifelse(judge == "Correct!", "", sprintf("Correct answer: '%d'", as.numeric(qq))))
                        })
                    } else {
                        output$feedback <- renderText({ 
                            "You should put a number in the box above!"
                        })
                    }
                }
                
            } else {
                output$feedback <- renderText({ "" })
            }
        })
    })
    
    # Kanji tab
    kanji <- reactiveValues(msg = NULL)
    observeEvent(list(kanji_level(), refresh_kanji()), {
        shinyjs::reset(id = "kanji_div")
        shinyjs::reset(id = "feedback_kanji")
        kanji$msg <- NULL
        
        if (kanji_level() == "JLPT N5") {
            df.kanji <<- kanji_n5
        }
        
        idx_kanji <<- sample(1:nrow(df.kanji), 1)
        
        # Question text
        output$kanji_q_text <- renderText({
            df.kanji$q_text[idx_kanji]
        })
        
        # Choice options
        output$kanji_choices <<- renderUI({
            kanji_options <<- sample(as.character(df.kanji[idx_kanji,3:7]))
            radioButtons("kanji_answer", "Choose the correct pronunciation: ", 
                         choices = c("Choose from below:", kanji_options))
        })
    })
        
    
    # Check answer and feedback
    observeEvent(list(input$check_kanji), {
        shinyjs::reset(id = "kanji_div")
        shinyjs::reset(id = "feedback_kanji")
        kanji$msg <- NULL
        
        judge <- ifelse(input$kanji_answer == df.kanji$corr_ans[idx_kanji], "Correct!", "Incorrect")
        kanji$msg <- paste0("<span style='color:", ifelse(judge == "Correct!", "green", "red"), ";font-size:20px'>", judge, "</span><br>   ",
                             ifelse(judge == "Correct!", "", sprintf("Correct answer: '%s'", df.kanji$corr_ans[idx_kanji])))
        
        if (is.null(kanji$msg)) return()
        output$feedback_kanji <- renderText({ kanji$msg })
    })
    
    
    # feedback_msg <- eventReactive(input$check_kanji, {
    #     req(input$check_kanji)
    #     if (!input$kanji_answer %in% kanji_options) {
    #         ""
    #     } else {
    #         judge <- ifelse(input$kanji_answer == df.kanji$corr_ans[idx_kanji], "Correct!", "Incorrect")
    #         paste0("<span style='color:", ifelse(judge == "Correct!", "green", "red"), ";font-size:20px'>", judge, "</span><br>   ",
    #                ifelse(judge == "Correct!", "", sprintf("Correct answer: '%s'", df.kanji$corr_ans[idx_kanji])))
    #     }
    # })
    # output$feedback_kanji <- feedback_msg()
    # output$feedback_kanji <- renderText({
    #     if (input$check_kanji == 0)
    #         return()
    #     if (!input$kanji_answer %in% kanji_options)
    #         return()
    #     isolate({
    #         judge <- ifelse(input$kanji_answer == df.kanji$corr_ans[idx_kanji], "Correct!", "Incorrect")
    #         paste0("<span style='color:", ifelse(judge == "Correct!", "green", "red"), ";font-size:20px'>", judge, "</span><br>   ",
    #                ifelse(judge == "Correct!", "", sprintf("Correct answer: '%s'", df.kanji$corr_ans[idx_kanji])))
    #     })
    # })
    
    # output$feedback_kanji <- renderText({
    #     kanji()$feedback_msg
    # })
    # observeEvent(input$check_kanji, {
    #     # input$check_kanji
    #     check_kanji()
    #     judge <- ifelse(input$kanji_answer == df.kanji$corr_ans[idx_kanji], "Correct!", "Incorrect")
    #     kanji(list(feedback_msg = paste0("<span style='color:", ifelse(judge == "Correct!", "green", "red"), ";font-size:20px'>", judge, "</span><br>   ",
    #                                      ifelse(judge == "Correct!", "", sprintf("Correct answer: '%s'", df.kanji$corr_ans[idx_kanji])))))
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
