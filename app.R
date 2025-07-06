library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(broom)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("📊 UAS - Aplikasi Prediksi Regresi Linier"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("train_data", "Upload Data Training (.csv)"),
      fileInput("test_data", "Upload Data Testing (.csv)"),
      uiOutput("select_x"),
      uiOutput("select_y"),
      actionButton("train_model", "Latih Model"),
      br(), br(),
      downloadButton("save_model", "Simpan Model"),
      fileInput("load_model", "Muat Model (.rds)")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview",
                 DTOutput("data_train"),
                 verbatimTextOutput("summary_train")
        ),
        tabPanel("Correlation Matrix",
                 plotOutput("cor_matrix")
        ),
        tabPanel("Exploratory Analysis",
                 uiOutput("scatter_x"),
                 uiOutput("scatter_y"),
                 plotOutput("scatter_plot")
        ),
        tabPanel("Model Regresi",
                 verbatimTextOutput("model_summary"),
                 plotOutput("pred_plot")
        ),
        tabPanel("Prediksi Data Baru",
                 DTOutput("prediction_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  train_data <- reactive({
    req(input$train_data)
    read_csv(input$train_data$datapath)
  })
  
  test_data <- reactive({
    req(input$test_data)
    read_csv(input$test_data$datapath)
  })
  
  output$data_train <- renderDT({
    datatable(train_data(), options = list(pageLength = 5))
  })
  
  output$summary_train <- renderPrint({
    summary(train_data())
  })
  
  output$select_x <- renderUI({
    req(train_data())
    selectInput("x_var", "Pilih Variabel X", choices = names(train_data()))
  })
  
  output$select_y <- renderUI({
    req(train_data())
    selectInput("y_var", "Pilih Variabel Y", choices = names(train_data()))
  })
  
  output$cor_matrix <- renderPlot({
    df <- train_data() %>% select(where(is.numeric))
    corrplot(cor(df, use = "complete.obs"), method = "color", type = "upper")
  })
  
  output$scatter_x <- renderUI({
    req(train_data())
    selectInput("scatter_x_var", "Sumbu X", choices = names(train_data()))
  })
  
  output$scatter_y <- renderUI({
    req(train_data())
    selectInput("scatter_y_var", "Sumbu Y", choices = names(train_data()))
  })
  
  output$scatter_plot <- renderPlot({
    req(input$scatter_x_var, input$scatter_y_var)
    ggplot(train_data(), aes_string(x = input$scatter_x_var, y = input$scatter_y_var)) +
      geom_point(aes_string(color = input$y_var), alpha = 0.7) +
      theme_minimal()
  })
  
  model <- reactiveVal(NULL)
  
  observeEvent(input$train_model, {
    df <- train_data()
    formula <- as.formula(paste(input$y_var, "~", input$x_var))
    model(lm(formula, data = df))
  })
  
  output$model_summary <- renderPrint({
    req(model())
    summary(model())
  })
  
  output$pred_plot <- renderPlot({
    req(model())
    df <- train_data()
    df$Predicted <- predict(model(), newdata = df)
    ggplot(df, aes_string(x = input$y_var, y = "Predicted")) +
      geom_point(color = "steelblue") +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(x = "Actual", y = "Predicted") +
      theme_minimal()
  })
  
  output$prediction_table <- renderDT({
    req(model(), test_data())
    df <- test_data()
    df$Predicted <- predict(model(), newdata = df)
    datatable(df, options = list(pageLength = 5))
  })
  
  output$save_model <- downloadHandler(
    filename = function() {
      "model_regresi.rds"
    },
    content = function(file) {
      saveRDS(model(), file)
    }
  )
  
  observeEvent(input$load_model, {
    req(input$load_model)
    model(readRDS(input$load_model$datapath))
  })
}

shinyApp(ui = ui, server = server)
