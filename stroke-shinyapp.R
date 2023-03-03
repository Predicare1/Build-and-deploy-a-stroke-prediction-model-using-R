#########################################################################
#########################################################################
## Deploying the stroke prediction model using R shiny
#########################################################################
#########################################################################

##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##
## ## Instructions: To run this app, click on the Run App button 
##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##

## Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidymodels)

## Load data and stroke model
complete_data <- read_rds("complete_data.rds")
model <- read_rds("stroke_model.rds")

## Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Stroke Prediction"),
  dashboardSidebar(
    selectInput("v_gender",
                "Gender:",
                c("Female" = 1,
                  "Male" = 0)),

    numericInput("v_age",
                 "Age:",
                 value = 50,
                 min = 1),

    selectInput("v_hypertension",
                "Hypertension:",
                c("Yes" = 1,
                  "No" = 0)),

    selectInput("v_heart_disease",
                "Heart Disease:",
                c("Yes" = 1,
                  "No" = 0)),

    selectInput("v_ever_married",
                "Are you married?",
                c("Yes" = 1,
                  "No" = 0)),

    selectInput("v_work_type",
                "Work Type:",
                unique(complete_data$work_type)),

    selectInput("v_residence_type",
                "Residence Type:",
                c("Urban" = 1,
                  "Rural" = 0)),

    numericInput(
      "v_avg_glucose_level",
      "Average Glucose Level:",
      value = round(mean(complete_data$avg_glucose_level), 2),
      min = round(min(complete_data$avg_glucose_level), 2)
    ),

    numericInput(
      "v_bmi",
      "BMI:",
      value = round(mean(complete_data$bmi), 2),
      min = round(min(complete_data$bmi), 2)
    ),

    selectInput("v_smoking_status",
                "Smoking Status:",
                c("Formerly Smoked/Smokes" = 1,
                  "Never Smoked" = 0)),

    actionButton("b_predict", "Predict!!")
  ),

  dashboardBody(fluidRow(
    #verbatimTextOutput("verb"),

    box(
      title = "Prediction",
      solidHeader = TRUE,
      collapsible = TRUE,
      width = 12,
      valueBoxOutput("prob_stroke", width = 6),
      valueBoxOutput("prob_not_stroke", width = 6)
    )

  )),
  title = "Stroke Prediction",
  skin = "black"

)

## Define required server logic 
server <- function(input, output) {
  reactive_df <-  reactive({
    tibble(
      "id" = 9046,
      "gender" = input$v_gender,
      "age" = input$v_age,
      "hypertension" = input$v_hypertension,
      "heart_disease" = input$v_heart_disease,
      "ever_married" = input$v_ever_married,
      "work_type" = input$v_work_type,
      "Residence_type" = input$v_residence_type,
      "avg_glucose_level" = input$v_avg_glucose_level,
      "bmi" = input$v_bmi,
      "smoking_status" = input$v_smoking_status
    )
  }) %>%
    bindEvent(input$b_predict)

  # To diagonize the input data to the model
  # output$verb <- renderPrint({ reactive_df() })


  output$prob_not_stroke <- renderValueBox({
    req(reactive_df())

    res <-  predict(model,
                    reactive_df(),
                    type = "prob") %>%
      pull(.pred_0) * 100

    valueBox(
      value = glue::glue("{round(res, 2)}%"),
      subtitle = "Probaility of not having a Stroke",
      icon = icon("info"),
      color = "green"
    )
  })

  output$prob_stroke <- renderValueBox({
    req(reactive_df())

    res <-  predict(model,
                    reactive_df(),
                    type = "prob") %>%
      pull(.pred_1) * 100

    valueBox(
      value = glue::glue("{round(res, 2)}%"),
      subtitle = "Probaility of having a Stroke",
      icon = icon("info"),
      color = "red"
    )
  })

}

## Run the application
shinyApp(ui = ui, server = server)
