library(shiny)
library(caret)
library(ggplot2)
library(DT)
library(dplyr)

# Helper Functions
detect_variable_types <- function(df) {
  sapply(df, function(col) {
    if (is.numeric(col)) return("Numeric")
    else if (is.factor(col)) return("Factor")
    else if (is.character(col)) return("Character")
    else if (inherits(col, "Date")) return("Date")
    else return("Other")
  })
}

impute_missing_values <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.numeric(col)) {
      col[is.na(col)] <- mean(col, na.rm = TRUE)
    } else if (is.factor(col) || is.character(col)) {
      col[is.na(col)] <- as.character(stats::na.omit(col)[1])
    }
    return(col)
  })
  return(df)
}

detect_outliers <- function(df, method = "IQR") {
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  outlier_flags <- data.frame(Row = 1:nrow(df))
  
  for (col in numeric_cols) {
    if (method == "IQR") {
      Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower <- Q1 - 1.5 * IQR
      upper <- Q3 + 1.5 * IQR
      outlier_flags[[col]] <- df[[col]] < lower | df[[col]] > upper
    } else if (method == "Z-Score") {
      z_scores <- (df[[col]] - mean(df[[col]], na.rm = TRUE)) / sd(df[[col]], na.rm = TRUE)
      outlier_flags[[col]] <- abs(z_scores) > 3
    }
  }
  return(outlier_flags)
}

train_model <- function(df, response, method) {
  unique_vals <- length(unique(df[[response]]))
  is_classification <- is.factor(df[[response]]) || is.character(df[[response]]) || unique_vals <= 5
  
  if (is_classification) {
    df[[response]] <- as.factor(df[[response]])
  } else {
    df[[response]] <- as.numeric(df[[response]])
  }
  
  trainIndex <- createDataPartition(df[[response]], p = .8, list = FALSE)
  trainData <- df[trainIndex, ]
  testData <- df[-trainIndex, ]
  
  if (is_classification && (nlevels(trainData[[response]]) < 2 || nlevels(testData[[response]]) < 2)) {
    stop("Training or testing data has less than 2 classes.")
  }
  
  tryCatch({
    model <- train(
      reformulate(setdiff(names(df), response), response),
      data = trainData,
      method = method,
      trControl = trainControl(method = "cv", number = 5)
    )
    
    predictions <- predict(model, newdata = testData)
    
    if (is_classification) {
      confusion <- confusionMatrix(predictions, testData[[response]])
    } else {
      confusion <- postResample(predictions, testData[[response]])
    }
    
    return(list(model = model, confusion = confusion))
  }, error = function(e) {
    stop(paste("Model training failed for method:", method, "with error:", e$message))
  })
}

compare_models <- function(df, response) {
  unique_vals <- length(unique(df[[response]]))
  is_classification <- is.factor(df[[response]]) || is.character(df[[response]]) || unique_vals <= 5
  
  methods <- if (is_classification) c("rf", "glm") else c("rf", "lm")
  
  results <- lapply(methods, function(m) train_model(df, response, m))
  
  if (is_classification) {
    accuracies <- sapply(results, function(res) res$confusion$overall["Accuracy"])
    best_index <- which.max(accuracies)
  } else {
    rmse_vals <- sapply(results, function(res) res$confusion["RMSE"])
    best_index <- which.min(rmse_vals)
  }
  
  return(list(
    best_method = methods[best_index],
    best_model = results[[best_index]]$model,
    best_confusion = results[[best_index]]$confusion
  ))
}

# Shiny App
ui <- fluidPage(
  titlePanel("Smart Data Explorer with Model Comparison and Outlier Detection"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File"),
      uiOutput("responseUI"),
      selectInput("outlier_method", "Outlier Detection Method", 
                  choices = c("IQR", "Z-Score"), selected = "IQR"),
      actionButton("preprocess", "Run Preprocessing"),
      actionButton("run", "Train Models")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Variable Types", DTOutput("varTypesTable")),
        tabPanel("Preprocessed Data", DTOutput("cleanedData")),
        tabPanel("Outliers", DTOutput("outlierTable")),
        tabPanel("Histograms", uiOutput("histogramPlots")),
        tabPanel("Model Summary", 
                 verbatimTextOutput("modelChoice"),
                 verbatimTextOutput("modelConfusion"))
      )
    )
  ),
  tags$hr(),
  tags$footer(
    tags$p("CM 703: Data Analysis | Jithmi Ponnamperuma_2507262", style = "text-align: center; color: grey; padding: 10px;")
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = TRUE)
  })
  
  output$responseUI <- renderUI({
    req(dataset())
    selectInput("response", "Response Variable", choices = names(dataset()))
  })
  
  variableTypes <- reactive({
    req(dataset())
    detect_variable_types(dataset())
  })
  
  output$varTypesTable <- renderDT({
    req(variableTypes())
    data.frame(Variable = names(variableTypes()), Type = unname(variableTypes()))
  })
  
  preprocessedData <- eventReactive(input$preprocess, {
    req(dataset())
    withProgress(message = 'Preprocessing data...', value = 0, {
      df <- impute_missing_values(dataset())
      return(df)
    })
  })
  
  output$cleanedData <- renderDT({
    req(preprocessedData())
    preprocessedData()
  })
  
  outlierData <- reactive({
    req(preprocessedData(), input$outlier_method)
    detect_outliers(preprocessedData(), input$outlier_method)
  })
  
  output$outlierTable <- renderDT({
    req(outlierData())
    datatable(outlierData(), options = list(scrollX = TRUE))
  })
  
  output$histogramPlots <- renderUI({
    req(preprocessedData())
    df <- preprocessedData()
    num_vars <- names(df)[sapply(df, is.numeric)]
    
    plot_output_list <- lapply(num_vars, function(var) {
      plotname <- paste0("plot_", var)
      output[[plotname]] <- renderPlot({
        ggplot(df, aes_string(x = var)) +
          geom_histogram(bins = 30, fill = "navyblue", color = "black") +
          theme_minimal() +
          labs(title = paste("Histogram of", var))
      })
      plotOutput(plotname)
    })
    do.call(tagList, plot_output_list)
  })
  
  results <- eventReactive(input$run, {
    req(preprocessedData(), input$response)
    withProgress(message = 'Training models...', value = 0, {
      compare_models(preprocessedData(), input$response)
    })
  })
  
  output$modelChoice <- renderPrint({
    req(results())
    paste("Best Model Selected:", toupper(results()$best_method))
  })
  
  output$modelConfusion <- renderPrint({
    req(results())
    results()$best_confusion
  })
}

shinyApp(ui = ui, server = server)
