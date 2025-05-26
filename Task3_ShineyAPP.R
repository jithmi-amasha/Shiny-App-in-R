# Load dataset
df <- read.csv('D:/MSC - Big Data Analytics/Semester 2/Data Analysis/Course Work/Bank_Churn.csv')

####TASK 3.1
variable_type = function(data){
  col_names = c(names(data))
  quantitative = c()
  qualitative = c()
  for(col in col_names){
    if (is.character(data[[col]]) || is.factor(data[[col]]) || is.logical(data[[col]])){
      qualitative = c(qualitative,col)
    }
    else{
      quantitative = c(quantitative,col)
    }
  }
  return(list(qualitative = qualitative, quantitative = quantitative))
}

variable_type(data = df)


###TASK 3.2

fill_na = function(data){
  variables = variable_type(data)
  missing_values = colSums(is.na(data))
  
  
  ##fill the missing values
  for (col in colnames(data)){
    if (col %in% variables$quantitative){
      data[[col]][is.na(df[[col]])] = mean(df[[col]],na.rm = TRUE)
    }
    else if (col %in% variables$quantitative){
      data[[col]][is.na(data[[col]])] <- names(sort(table(data[[col]], useNA = "ifany"), decreasing = TRUE))[1]
    }
  }
  return(list(data=data,missing_values=missing_values))
  
  
}



##TASK 3.3
ident_univar_outliers <- function(data, method = "IQR") { 
  variable <- variable_type(data)
  outlier_values <- list()
  
  for (col in variable$quantitative) {
    x <- data[[col]]
    
    if (method == "IQR") {
      q1 <- quantile(x, 0.25, na.rm = TRUE) 
      q3 <- quantile(x, 0.75, na.rm = TRUE) 
      iqr <- q3 - q1  
      lower <- q1 - 1.5 * iqr 
      upper <- q3 + 1.5 * iqr 
      outliers <- x[x < lower | x > upper]
    } else if (method == "Z") {
      z_scores <- scale(x)
      outliers <- x[abs(z_scores) > 3]
    } else {
      stop("Unsupported method. Choose 'IQR' or 'Z'.")
    }
    
    # Add only if outliers exist
    if (length(outliers) > 0) {
      outlier_values[[col]] <- outliers
    }
  }
  
  return(outlier_values)
}

univar_outliers <- ident_univar_outliers(data = df,method = "IQR")
univar_outliers









###Task 3.4
visualize <- function(data,method="IQR") {
  variable <- variable_type(data)
  filling_missing_values = fill_na(data)
  data = filling_missing_values$data
  outlier_data = ident_univar_outliers(data,method)
  plots = list()
  library(ggplot2)
  library(dplyr)
  
  for(var in names(data)) {
    if (var %in% variable$quantitative) {
      pl <- ggplot(data, aes_string(x = var)) +
        geom_histogram( color = 'green', fill = 'pink', alpha = 1,bins = 20) +
        xlab(var) +
        ylab('Count') +ggtitle(var)
      print(pl)
    } else {
      pl <- ggplot(data, aes_string(x = var)) +
        geom_bar() +
        ggtitle(var)
      print(pl)
    }
    plots[[var]] = pl
  }
  return(plots)
}

visualize(data = df)

##TASK 3.5
predictive_model = function(data,response,method="IQR"){
  variable <- variable_type(data)
  filling_missing_values = fill_na(data)
  data = filling_missing_values$data
  outlier_data = ident_univar_outliers(data,method)
  plots = visualize(data)
  # Specify response variable
  response = response
  
  # Check if response variable is in the dataset
  if (!(response %in% names(data))) {
    stop("Given variable is not in the dataset")
    
  } else if (is.numeric(data[[response]])) {
    print("Regression task: Numeric response variable detected")
    
    # Convert character columns to numeric (factor encoding)
    for (col in names(data)) {
      if (is.character(data[[col]])) {
        data[[col]] <- as.numeric(as.factor(data[[col]]))
      }
    }
    
    # Regression task
    library(caTools)
    set.seed(101)
    sample <- sample.split(data[[response]], SplitRatio = 0.7)
    train <- subset(data, sample == TRUE)
    test <- subset(data, sample == FALSE)
    
    # Train the model
    model <- lm(as.formula(paste(response, "~ .")), data = train)
    print(summary(model))
    
    # Predict and evaluate
    predictions <- predict(model, test)
    actuals <- test[[response]]
    results <- data.frame(predicted = predictions, actual = actuals)
    print(results)
    
    # Mean Squared Error
    mse <- mean((results$actual - results$predicted)^2)
    print(paste("MSE:", mse))
    
    # Root Mean Squared Error
    rmse <- sqrt(mse)
    print(paste("RMSE:", rmse))
    
    # R-squared
    SSE <- sum((results$predicted - results$actual)^2)
    SST <- sum((mean(test[[response]]) - results$actual)^2)
    R2 <- 1 - SSE/SST
    print(paste("R-squared:", R2))
    
    
  } else if ((is.factor(data[[response]]) && length(levels(data[[response]])) == 2) ||
             (is.character(data[[response]]) && length(unique(data[[response]])) == 2)) {
    
    print("Binary classification task detected")
    
    # Convert character columns to numeric (including response variable if it's character)
    for (col in colnames(data)) {
      if (is.character(data[[col]])) {
        data[[col]] <- as.numeric(as.factor(data[[col]]))
      }
    }
    
    # Set response variable as factor for classification
    data[[response]] <- as.factor(data[[response]])
    library(ROSE)
    
    # Handle class imbalance using oversampling
    formula_str <- as.formula(paste(response, "~ ."))
    oversample_result <- ovun.sample(formula_str, data = data, method = "over") 
    data <- oversample_result$data  # Assign back to data
    
    
    # Train-Test Split
    set.seed(101)
    split <- sample.split(data[[response]], SplitRatio = 0.8)
    train <- subset(data, split == TRUE)
    test <- subset(data, split == FALSE)
    
    # Train a decision tree model
    library(rpart)
    decision_tree <- rpart(formula_str, data = train, method = "class")
    
    # Predictions on the test set
    predictions <- predict(decision_tree, newdata = test, type = "class")
    
    # Evaluate model performance
    library(caret)
    conf_matrix <- confusionMatrix(predictions, test[[response]])
    print(conf_matrix)
    
  } else {
    stop("Please select a binary or continuous response variable.")
  }
  
}





library(shiny)


ui <- fluidPage(
  titlePanel("Predictive Model Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("response_ui"),
      selectInput("method", "Select Outlier Detection Method", choices = c("IQR", "Z")),
      actionButton("run", "Run Model")
    ),
    
    mainPanel(
      verbatimTextOutput("model_output"),
      h4("Diagnostic / Evaluation Plots:"),
      uiOutput("plot_ui")
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$response_ui <- renderUI({
    req(dataset())
    selectInput("response", "Select Response Variable", choices = names(dataset()))
  })
  
  results <- eventReactive(input$run, {
    req(dataset(), input$response)
    capture.output({
      predictive_model(dataset(), input$response, input$method)
    })
  })
  
  output$model_output <- renderPrint({
    req(results())
    cat(paste(results(), collapse = "\n"))
  })
  
  # Assuming `visualize(data)` returns a list of ggplot objects
  output$plot_ui <- renderUI({
    plots <- visualize(fill_na(dataset())$data)
    plot_output_list <- lapply(seq_along(plots), function(i) {
      plotname <- paste0("plot", i)
      output[[plotname]] <- renderPlot({ plots[[i]] })
      plotOutput(plotname)
    })
    do.call(tagList, plot_output_list)
  })
}

shinyApp(ui = ui, server = server)

