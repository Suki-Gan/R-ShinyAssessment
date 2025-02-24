library(shiny)
library(readxl)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
library(janitor)
library(tibble)

# UI
ui <- fluidPage(
  titlePanel("R-Shiny Assignment"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Excel File", accept = ".xlsx"),
      numericInput("tail_factor", "Enter Tail Factor:", value = 1.1, step = 0.1),
      actionButton("process", "Process Data"),
    ),
    mainPanel(
      h3(textOutput("output_title")),
      DTOutput("claims_table"),
      plotOutput("claims_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store data and metadata
  cal_data <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  file_name <- reactiveVal(NULL)
  # Process the uploaded file
  observeEvent(input$process, {
    req(input$file)
    df <- read_excel(input$file$datapath) %>% clean_names()
    
    if (!all(c("loss_year", "development_year", "amount_of_claims_paid") %in% colnames(df))) {
      showNotification("Error: Missing required columns in the uploaded file.", type = "error")
      return()
    }
    
    loss_years <- sort(unique(df$loss_year))
    dvlp_years <- sort(unique(df$development_year))
    N <- length(dvlp_years)
    
    df_cumulative <- data.frame(matrix(NA, nrow = length(loss_years), ncol = N + 1), row.names = loss_years)
    colnames(df_cumulative) <- c(paste("Development Year", 1:N), paste("Development Year", N + 1))
    
    for (j in 1:N) {
      for (i in 1:(length(loss_years) - j + 1)) {
        df_cumulative[i, j] <- sum(df$amount_of_claims_paid[df$loss_year == loss_years[i] & df$development_year <= dvlp_years[j]])
      }
      if (N - j + 2 <= length(loss_years)) {
        for (i in (N - j + 2):length(loss_years)) {
          df_cumulative[i, j] <- sum(df_cumulative[1:(length(loss_years) - j + 1), j], na.rm = TRUE) / 
            sum(df_cumulative[1:(length(loss_years) - j + 1), j - 1], na.rm = TRUE) * df_cumulative[i, j - 1]
        }
      }
    }
    TailFactor <- input$tail_factor
    df_cumulative[, N + 1] <- df_cumulative[, N] * TailFactor
    cal_data(df_cumulative)
    file_name(input$file$name)
  })
  

  # Render the output title
  output$output_title <- renderText({
    if (!is.null(file_name())) {
      paste("File Output:", basename(file_name()))
    } else if (!is.null(input$file)) {
      paste("File Output:", input$file$name)
    } else {
      "No File Selected"
    }
  })
  
  # Render the claims table
  output$claims_table <- renderDT({
    req(cal_data())
    datatable(
      data = cal_data(),
      options = list(pageLength = 10),
      rownames = TRUE,
      colnames = c("Loss Year", colnames(cal_data())),
      caption = htmltools::tags$caption("Cumulative Paid Claims ($)", style = "font-size:16px; color:black;")
    ) %>%
      formatCurrency(columns = 1:ncol(cal_data()), currency = "", interval = 3, mark = ",")
  })
  
  # Render the claims plot
  output$claims_plot <- renderPlot({
    req(cal_data())
    plot_data <- cal_data() %>%
      rownames_to_column(var = "Loss Year") %>%
      pivot_longer(cols = -`Loss Year`, names_to = "Development Year", values_to = "Cumulative Claims") %>%
      mutate(`Development Year` = as.numeric(gsub("Development Year ", "", `Development Year`)))
    
    ggplot(plot_data, aes(x = `Development Year`, y = `Cumulative Claims`, color = `Loss Year`)) +
      geom_line(size = 1, linetype = "solid") +
      geom_point(size = 2) +
      labs(
        title = "Cumulative Paid Claims ($)",
        x = "Development Year",
        y = "Cumulative Claims ($)",
        color = "Loss Year"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}
shinyApp(ui = ui, server = server)
