options(warn=-1)

setwd('C:\\Users\\ABC\\Documents\\final deployment birender nb')
library(caret)
library(shiny)
library(LiblineaR)
library(readr)
library(dplyr)
library(e1071)
library(tm)
library(SnowballC)
library(tm.plugin.mail)
library(stringr)
library(tidytext)
library(pander)

#Text Clean Function.
clean.text = function(x)
{
    # tolower
    x = tolower(x)
    # remove punctuation
    x = gsub("[[:punct:]]", "", x)
    # remove numbers
    x = gsub("[[:digit:]]", "", x)
    # remove tabs
    x = gsub("[ |\t]{2,}", "", x)
    # remove blank spaces at the beginning
    x = gsub("^ ", "", x)
    # remove blank spaces at the end
    x = gsub(" $", "", x)
    # remove stop words
    x = removeWords(x,stopwords("en"))
    return(x)
}

load("model.rds") # Load saved model

shinyServer(function(input, output) {
    options(shiny.maxRequestSize = 800*1024^2)   #Maximum file size upload limit 80MB.
    
    output$sample_input_data_heading = renderUI({   # show only if data has been uploaded
        inFile <- input$file1
        if (is.null(inFile)){
            return(NULL)
        }
        else{
            tags$h2('Sample data', align = "center")
        }
    })
    
    output$sample_DTM_heading = renderUI({   # DTM Heading
        inFile <- input$file1
        if (is.null(inFile)){
            return(NULL)
        }
        else{
            tags$h2('Sample Document Term Matrix', align = "center")
        }
    })
    
    
    output$ui.action <- renderUI({
        inFile <- input$file1
        if (is.null(inFile)) return()
        fluidRow(
            column(6, align="center", offset = 3,
                   downloadButton('downloadData',label = "'Download Prediction'
                "#button { vertical-align- middle; height- 70px; width- 100%; font-size- 40px;}")
                   )))
    })
    
    
    output$sample_input_data = renderTable({    # show sample of uploaded data
        inFile <- input$file1
        
        if (is.null(inFile)){
            return(NULL)
        }
        else{
            input_data =  readr::read_csv(input$file1$datapath)
            input_data = input_data['content']
            head(input_data)
        }
    })
    
    output$sample_DTM_data = renderTable({    # show sample DTM
        inFile <- input$file1
        
        if (is.null(inFile)){
            return(NULL)
        }
        else{
            input_data =  readr::read_csv(input$file1$datapath)
            #input_data = input_data[c(3060:4060),c(4)]
            
            #Pre-Processing
            corpus = VCorpus(VectorSource(input_data))
            corpus = tm_map(corpus, content_transformer(tolower))
            corpus = tm_map(corpus, removeNumbers)
            corpus = tm_map(corpus, removePunctuation)
            corpus = tm_map(corpus, removeWords, stopwords("english"))
            corpus = tm_map(corpus, stemDocument)
            corpus = tm_map(corpus, stripWhitespace)
            dtm = DocumentTermMatrix(corpus)
            dtm = removeSparseTerms(dtm, 0.95)
            dtm <- tidy(dtm)
            head(dtm)
            
        }
    })
    
    predictions<-reactive({
        
        inFile <- input$file1
        
        if (is.null(inFile)){
            return(NULL)
        }
        else{
            withProgress(message = 'Predictions in progress. Please wait ...', {
                input_data =  readr::read_csv(input$file1$datapath)
               # input_data = input_data[c(1:2000),4]
                input_data = as.data.frame(input_data)
                
                #Pre-Processing
                cleanText <- clean.text(input_data[,1])
                
                # Build Corpus
                corpus <- Corpus(VectorSource(cleanText))
                
                # Build  Document Term Matrix
                dtm <- DocumentTermMatrix(corpus)
                
                # Convert DTM to Dataframe
                dtm.df <- as.data.frame(data.matrix(dtm),stringsAsFactors=FALSE)
                
                # Remove features with total frequency less than 3
                dtm.new <- dtm.df[,colSums(dtm.df) > 2]
                
                
                #End of Pre-Processing.
                
                #Pridiction of input data.
                email_predict2 <- predict(model, dtm.new)
                input_data_with_prediction1 = cbind(input_data,email_predict2)
                input_data_with_prediction1
                
            })
        }
    })
    
    
    output$sample_prediction_heading = renderUI({  # show only if data has been uploaded
        inFile <- input$file1
        
        if (is.null(inFile)){
            return(NULL)
        }
        else{
            tags$h2('Sample predictions',align = "center")
        }
    })
    
    #Downloadable csv of predictions ----
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("input_data_with_predictions1", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(predictions(), file, row.names = FALSE)
        })
    
})

