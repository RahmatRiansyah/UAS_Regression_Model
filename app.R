library(shiny)
library(ggplot2)
library(DT)
library(shinythemes)
library(corrplot)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("📈 Aplikasi Prediksi Regresi Linier - UAS"),
  
  sidebarLayout(
    sidebarPanel(
      tags$h4("Langkah 1: Upload Data Training"),
      fileInput("train_file", "Upload File CSV", accept = ".csv"),
      
      uiOutput("var_select"),
      actionButton("train_model", "Latih Model", class = "btn btn-primary"),
      
      tags$hr(),
      tags$h4("Langkah 2: Simpan / Muat Model"),
      downloadButton("save_model", "💾 Simpan Model"),
      fileInput("load_model", "📂 Muat Model (.rds)"),
      
      tags$hr(),
      tags$h4("Langkah 3: Upload Data Testing"),
      fileInput("test_file", "Upload Data Testing (.csv)", accept = ".csv"),
      actionButton("predict_button", "Prediksi", class = "btn btn-success")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("📊 Data Preview", DTOutput("train_table")),
        tabPanel("📉 Korelasi", plotOutput("correlation_plot")),
        tabPanel("🔍 Eksplorasi", plotOutput("exploratory_plot")),
        tabPanel("📈 Model", verbatimTextOutput("model_summary")),
        tabPanel("🔮 Prediksi", DTOutput("prediction_table"), plotOutput("prediction_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  data_train <- reactiveVal()
  data_test <- reactiveVal()
  model_lm <- reactiveVal()
  pred_result <- reactiveVal(NULL)
  data_train <- reactiveVal()
  data_test <- reactiveVal()
  model_lm <- reactiveVal()
  pred_trigger <- reactiveVal(0)
  
  observeEvent(input$train_file, {
    df <- read.csv(input$train_file$datapath)
    data_train(df)
  })
  
  output$var_select <- renderUI({
    req(data_train())
    tagList(
      selectInput("x_var", "Pilih Variabel X (Target)", choices = names(data_train())),
      selectInput("y_var", "Pilih Variabel Y (Prediktor)", choices = names(data_train()), multiple = TRUE)
    )
  })
  
  observeEvent(input$train_model, {
    req(data_train(), input$x_var, input$y_var)
    predictors <- paste(input$y_var, collapse = " + ")
    formula <- as.formula(paste(input$x_var, "~", predictors))
    model <- lm(formula, data = data_train())
    model_lm(model)
  })
  
  output$train_table <- renderDT({
    req(data_train())
    datatable(data_train())
  })
  
  output$correlation_plot <- renderPlot({
    req(data_train())
    num_data <- data_train()[, sapply(data_train(), is.numeric)]
    corr_matrix <- cor(num_data, use = "complete.obs")
    corrplot(corr_matrix, method = "color", type = "upper",
             tl.col = "black", addCoef.col = "black",
             number.cex = 0.7, tl.cex = 0.8)
  })
  
  output$exploratory_plot <- renderPlot({
    req(data_train(), input$x_var, input$y_var)
    y_first <- input$y_var[1]
    ggplot(data_train(), aes_string(x = y_first, y = input$x_var)) +
      geom_point(aes_string(color = input$x_var), size = 3) +
      scale_color_gradient2(low = "#1f77b4", mid = "#9b59b6", high = "#e74c3c",
                            midpoint = mean(data_train()[[input$x_var]], na.rm = TRUE)) +
      theme_minimal() +
      labs(title = paste("Plot", input$x_var, "vs", y_first), color = input$x_var)
  })
  
  output$model_summary <- renderPrint({
    req(model_lm())
    summary(model_lm())
  })
  
  output$save_model <- downloadHandler(
    filename = function() { "model_uas.rds" },
    content = function(file) {
      saveRDS(model_lm(), file)
    }
  )
  
  observeEvent(input$load_model, {
    req(input$load_model)
    model <- readRDS(input$load_model$datapath)
    model_lm(model)
    showNotification("✅ Model berhasil dimuat. Silakan upload data testing lalu klik tombol Prediksi.")
  })
  
  observeEvent(input$test_file, {
    df <- read.csv(input$test_file$datapath)
    data_test(df)
  })
  
  observeEvent(input$predict_button, {
    req(model_lm(), data_test())
    prediction <- predict(model_lm(), newdata = data_test())
    result <- cbind(data_test(), Prediksi = prediction)
    pred_result(result)
  })
  
  output$prediction_table <- renderDT({
    req(pred_result())
    datatable(pred_result())
  })
  
  output$prediction_plot <- renderPlot({
    req(pred_result(), input$x_var)
    actual <- pred_result()[[input$x_var]]
    predicted <- pred_result()[["Prediksi"]]
    ggplot(data.frame(Actual = actual, Predicted = predicted), aes(x = Actual, y = Predicted)) +
      geom_point(aes(color = Predicted), size = 3) +
      scale_color_gradient(low = "#3498db", high = "#e74c3c") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      theme_minimal() +
      labs(title = "Prediksi vs Aktual")
  })
}

shinyApp(ui, server)
