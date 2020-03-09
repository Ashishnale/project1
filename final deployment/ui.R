library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel(h1("Incident Prediction", align = "center")),
    tags$hr(),
    fluidRow(
        column(6, align="center", offset = 3,
               fileInput('file1',label = "'Upload test data in csv format'
                "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
               ))),
    tags$hr(),
    uiOutput("sample_input_data_heading"),
    tableOutput("sample_input_data"),
    uiOutput("sample_DTM_heading"),
    fluidRow(
        column(6, align="center", offset = 3,
               tableOutput("sample_DTM_data")
        )),
    
    #uiOutput("sample_prediction_heading"),
    #uiOutput("sample_predictions"),
    uiOutput('ui.action'),
    #downloadButton("downloadData", em('Download Predictions',style="text-align:center;color:blue;font-size:150%")),
))