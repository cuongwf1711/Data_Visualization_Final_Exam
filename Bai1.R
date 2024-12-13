library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  div(
    h1("User Activity Visualization And Summary", align = "center"),
    style = "margin: 30px;"
  ),

  div(
    style = "display: flex; justify-content: center; align-items: center; margin: 20px; text-align: center;",
    div(
      div(
        h4("Select one of the history logs:", style = "text-align: center; margin-bottom: 30px;")
      ),
      radioButtons(
        "selected_file",
        label = NULL,
        choices = list(
          "historytrial2037" = "historytrial2037.csv",
          "historytrialdir2037" = "historytrialdir2037.csv"
        ),
        selected = "historytrial2037.csv"
      ),
      textOutput("log_description"),
      style = "text-align: left; width: 50%; padding: 10px; border: 1px solid #ddd; border-radius: 8px;"
    )
  ),

  div(
    plotOutput("activity_plot", width = "100%"),
    style = "text-align: center;"
  ),

  div(
    h3("Summary", align = "center"),
    div(
      tableOutput("summary_table"),
      style = "display: flex; justify-content: center; font-size: 22px; padding: 10px;"
    )
  )
)

server <- function(input, output) {

  load_data <- reactive({
    data <- read.csv(
      paste0("/home/c/Downloads/Truc quan hoa du lieu/Bai1_TrucQuanDuLieu/Project_TrucQuanDuLieu/", input$selected_file)
    )
    data$Start.Date <- as.Date(data$Start.Time)
    return(data)
  })

  output$log_description <- renderText({
    paste("Selected log:", input$selected_file)
  })

  output$activity_plot <- renderPlot({
    user_data <- load_data()

    summarized_data <- user_data %>%
      group_by(Start.Date, User) %>%
      summarise(Activity_Count = n(), .groups = "drop")

    user_list <- unique(summarized_data$User)

    user_colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(length(user_list))

    color_mapping <- setNames(user_colors, user_list)

    ggplot(summarized_data, aes(x = Start.Date, y = Activity_Count, fill = User)) +
      geom_bar(stat = "identity") +
      theme_bw() +
      labs(
        title = "User Activity by Day",
        x = "Date",
        y = "Number of Activities",
        fill = "User"
      ) +
      scale_fill_manual(values = color_mapping) +
      scale_x_date(
        date_breaks = "1 day",
        date_labels = "%Y/%m/%d"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })

  output$summary_table <- renderTable({
    user_data <- load_data()

    summary_data <- user_data %>%
      group_by(Start.Date) %>%
      summarise(
        `Unique Users` = n_distinct(User),
        `Total Searches` = n(),
        .groups = "drop"
      ) %>%
      rename(Date = Start.Date) %>%
      mutate(Date = as.character(Date))

    return(summary_data)
  })
}

runApp(shinyApp(ui = ui, server = server))
