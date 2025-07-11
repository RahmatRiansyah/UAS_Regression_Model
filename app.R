library(shiny)
library(ggplot2)
library(DT)
library(shinythemes)
library(corrplot)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("📈 Aplikasi Prediksi Regresi Linier Berganda - UAS"),
  
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
  
  # Step 1: Upload data training
  observeEvent(input$train_file, {
    df <- read.csv(input$train_file$datapath)
    data_train(df)
  })
  
  # Step 2: UI untuk memilih Y dan X (bisa banyak X)
  output$var_select <- renderUI({
    req(data_train())
    tagList(
      selectInput("y_var", "Pilih Variabel Y (Target)", choices = names(data_train()), multiple = FALSE),
      selectInput("x_var", "Pilih Variabel X (Prediktor)", choices = names(data_train()), multiple = TRUE)
    )
  })
  
  # Step 3: Latih model regresi
  observeEvent(input$train_model, {
    req(data_train(), input$y_var, input$x_var)
    
    # Validasi: Y tidak boleh termasuk dalam X
    if (input$y_var %in% input$x_var) {
      showNotification("❌ Variabel Y tidak boleh sama dengan salah satu variabel X!", type = "error")
      return()
    }
    
    # Bangun model regresi berganda
    predictors <- paste(input$x_var, collapse = " + ")
    formula <- as.formula(paste(input$y_var, "~", predictors))
    model <- lm(formula, data = data_train())
    model_lm(model)
  })
  
  # Tampilkan data training
  output$train_table <- renderDT({
    req(data_train())
    datatable(data_train())
  })
  
  # Tampilkan korelasi numerik
  output$correlation_plot <- renderPlot({
    req(data_train())
    num_data <- data_train()[, sapply(data_train(), is.numeric)]
    corr_matrix <- cor(num_data, use = "complete.obs")
    corrplot(corr_matrix, method = "color", type = "upper",
             tl.col = "black", addCoef.col = "black",
             number.cex = 0.7, tl.cex = 0.8)
  })
  
  # Scatter plot eksplorasi dengan 1 X pertama
  output$exploratory_plot <- renderPlot({
    req(data_train(), input$y_var, input$x_var)
    x_first <- input$x_var[1]
    ggplot(data_train(), aes_string(x = x_first, y = input$y_var)) +
      geom_point(aes_string(color = input$y_var), size = 3) +
      scale_color_gradient2(low = "#1f77b4", mid = "#9b59b6", high = "#e74c3c",
                            midpoint = mean(data_train()[[input$y_var]], na.rm = TRUE)) +
      theme_minimal() +
      labs(title = paste("Plot", input$y_var, "vs", x_first), color = input$y_var)
  })
  
  # Ringkasan model
  output$model_summary <- renderPrint({
    req(model_lm())
    summary(model_lm())
  })
  
  # Simpan model
  output$save_model <- downloadHandler(
    filename = function() { "model_uas.rds" },
    content = function(file) {
      saveRDS(model_lm(), file)
    }
  )
  
  # Muat model
  observeEvent(input$load_model, {
    req(input$load_model)
    model <- readRDS(input$load_model$datapath)
    model_lm(model)
    showNotification("✅ Model berhasil dimuat. Silakan upload data testing lalu klik tombol Prediksi.")
  })
  
  # Upload data testing
  observeEvent(input$test_file, {
    df <- read.csv(input$test_file$datapath)
    data_test(df)
  })
  
  # Prediksi nilai dari data testing
  observeEvent(input$predict_button, {
    req(model_lm(), data_test(), input$x_var)
    
    # Validasi: semua kolom X tersedia di data_test
    missing_cols <- setdiff(input$x_var, names(data_test()))
    if (length(missing_cols) > 0) {
      showNotification(
        paste("❌ Kolom berikut tidak ditemukan di data testing:", paste(missing_cols, collapse = ", ")),
        type = "error"
      )
      return()
    }
    
    # Lakukan prediksi
    prediction <- predict(model_lm(), newdata = data_test())
    result <- cbind(data_test(), Prediksi = prediction)
    pred_result(result)
  })
  
  # Tabel prediksi
  output$prediction_table <- renderDT({
    req(pred_result())
    datatable(pred_result())
  })
  
  # Plot prediksi vs aktual
  output$prediction_plot <- renderPlot({
    req(pred_result(), input$y_var)
    actual <- pred_result()[[input$y_var]]
    predicted <- pred_result()[["Prediksi"]]
    ggplot(data.frame(Aktual = actual, Prediksi = predicted), aes(x = Aktual, y = Prediksi)) +
      geom_point(aes(color = Prediksi), size = 3) +
      scale_color_gradient(low = "#3498db", high = "#e74c3c") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      theme_minimal() +
      labs(title = "Prediksi vs Aktual")
  })
}

shinyApp(ui, server)
