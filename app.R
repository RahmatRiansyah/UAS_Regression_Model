library(shiny)
library(caret)
library(ggplot2)
library(DT)
library(shinythemes)
library(corrplot)

ui <- fluidPage(
  theme = shinytheme("flatly"),

  titlePanel("✨ Aplikasi Prediksi Regresi Linier ✨"),

  sidebarLayout(
    sidebarPanel(
      tags$h4("Langkah 1: Upload Data Training"),
      fileInput("train_file", "Upload Data Training (.csv)", accept = ".csv"),

      uiOutput("var_select_ui"),

      actionButton("train_model", "Latih Model", class = "btn btn-primary"),

      br(), br(),
      tags$hr(),

      tags$h4("Langkah 2: Upload Data Testing"),
      fileInput("test_file", "Upload Data Testing (.csv)", accept = ".csv"),
      actionButton("predict_button", "Prediksi Data Testing", class = "btn btn-success"),

      br(), br(),
      tags$hr(),

      tags$h4("Langkah 3: Simpan / Muat Model"),
      downloadButton("save_model", "💾 Simpan Model"),
      fileInput("load_model", "📂 Muat Model (.rds)")
    ),

    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("📊 Data Training", DTOutput("train_preview")),
        tabPanel("📉 Korelasi", plotOutput("correlation_plot")),
        tabPanel("🔍 Eksplorasi", plotOutput("exploratory_plot")),
        tabPanel("📈 Model", verbatimTextOutput("model_summary")),
        tabPanel("🧪 Data Testing & Prediksi", DTOutput("prediction_table")),
        tabPanel("📉 Plot", plotOutput("prediction_plot"))
      )
    )
  )
)

server <- function(input, output, session) {
  train_data <- reactiveVal()
  test_data <- reactiveVal()
  model_lm <- reactiveVal()

  observeEvent(input$train_file, {
    df <- read.csv(input$train_file$datapath)
    train_data(df)

    updateSelectInput(session, "target_var", choices = names(df))
    updateSelectInput(session, "predictor_var", choices = names(df))
  })

  output$var_select_ui <- renderUI({
    req(train_data())
    tagList(
      selectInput("target_var", "Pilih Variabel Target (Y)", choices = names(train_data())),
      selectInput("predictor_var", "Pilih Variabel Prediktor (X)", choices = names(train_data()))
    )
  })

  observeEvent(input$train_model, {
    req(train_data(), input$target_var, input$predictor_var)

    formula_text <- paste(input$target_var, "~", input$predictor_var)
    model <- lm(as.formula(formula_text), data = train_data())
    model_lm(model)
  })

  output$train_preview <- renderDT({
    req(train_data())
    datatable(train_data(), options = list(pageLength = 5))
  })

  output$model_summary <- renderPrint({
    req(model_lm())
    summary(model_lm())
  })

  observeEvent(input$test_file, {
    df <- read.csv(input$test_file$datapath)
    test_data(df)
  })

  output$prediction_table <- renderDT({
    req(model_lm(), test_data())
    pred <- predict(model_lm(), newdata = test_data())
    result <- cbind(test_data(), Prediksi = pred)
    datatable(result, options = list(pageLength = 5))
  })

  output$prediction_plot <- renderPlot({
    req(model_lm(), test_data())
    pred <- predict(model_lm(), newdata = test_data())
    actual <- test_data()[[input$target_var]]
    df_plot <- data.frame(Actual = actual, Predicted = pred)

    ggplot(df_plot, aes(x = Actual, y = Predicted)) +
      geom_point(aes(color = Predicted), size = 3) +
      scale_color_gradient(low = "#3498db", high = "#e74c3c") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
      theme_minimal(base_size = 14) +
      labs(
        title = "📉 Plot Prediksi vs Aktual",
        subtitle = "Gradasi warna: biru = prediksi rendah, merah = prediksi tinggi",
        x = "Nilai Aktual",
        y = "Nilai Prediksi",
        color = "Prediksi"
      )
  })

  output$correlation_plot <- renderPlot({
    req(train_data())
    numeric_data <- train_data()[, sapply(train_data(), is.numeric)]
    corr_matrix <- cor(numeric_data, use = "complete.obs")
    corrplot(corr_matrix, method = "color", type = "upper", tl.cex = 0.8)
  })

  output$exploratory_plot <- renderPlot({
    req(train_data(), input$target_var, input$predictor_var)
    ggplot(train_data(), aes_string(x = input$predictor_var, y = input$target_var)) +
      geom_point(aes_string(color = input$target_var), size = 3) +
      scale_color_gradient(low = "blue", high = "red") +
      theme_minimal(base_size = 14) +
      labs(title = paste("🔍 Scatter Plot:", input$predictor_var, "vs", input$target_var))
  })

  output$save_model <- downloadHandler(
    filename = function() { "model_regresi.rds" },
    content = function(file) {
      saveRDS(model_lm(), file)
    }
  )

  observeEvent(input$load_model, {
    req(input$load_model)
    loaded_model <- readRDS(input$load_model$datapath)
    model_lm(loaded_model)
    showNotification("✅ Model berhasil dimuat. Silakan upload data testing lalu klik tombol Prediksi.", type = "message")
  })
}

shinyApp(ui, server)
