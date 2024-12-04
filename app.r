library(shiny)
library(quantmod)
library(DT)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Yahoo Finance Stock Data"),
  sidebarLayout(
    sidebarPanel(
      textInput("symbol", "Enter Stock Symbol:", value = "AAPL"),
      actionButton("get_data", "Get Data")
    ),
    mainPanel(
      DTOutput("stock_table"),
      plotOutput("stock_chart")
    )
  )
)

server <- function(input, output) {
  stock_data <- eventReactive(input$get_data, {
    symbol <- input$symbol
    getSymbols(symbol, src = "yahoo", auto.assign = FALSE)
  })
  
  output$stock_table <- renderDT({
    data <- stock_data()
    datatable(data.frame(Date = index(data), coredata(data)), options = list(pageLength = 10))
  })
  
  output$stock_chart <- renderPlot({
    data <- stock_data()
    ggplot(data.frame(Date = index(data), coredata(data)), aes(x = Date, y = data[, "AAPL.Close"])) +
      geom_line() +
      labs(title = paste("Closing Prices for", input$symbol), x = "Date", y = "Closing Price") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)