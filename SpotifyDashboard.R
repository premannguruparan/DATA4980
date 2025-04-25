library(shiny)
library(ggplot2)
library(ggridges)
library(readxl)
library(dplyr)
library(tools)
library(DT)
library(scales)

ui <- fluidPage(
  titlePanel("Spotify Streaming Explorer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload an Excel File", accept = c(".xls", ".xlsx")),
      conditionalPanel(
        condition = "input.tabs == 'Top Years'",
        sliderInput("percent_threshold", "Top % of Streams:", min = 1, max = 100, value = 10),
        sliderInput("num_years", "Number of Years to Show:", min = 1, max = 30, value = 15)
      ),
      conditionalPanel(
        condition = "input.tabs == 'BPM Ridge Plot'",
        sliderInput("percent_split", "Top vs Bottom % Split:", min = 1, max = 99, value = 10)
      ),
      conditionalPanel(
        condition = "input.tabs == 'Top Artists'",
        sliderInput("num_artists", "Number of Top Artists to Display:", min = 5, max = 50, value = 15)
      ),
      conditionalPanel(
        condition = "input.tabs == 'Streams by Song Age'",
        sliderInput("age_bucket", "Max Song Age (in years):", min = 5, max = 40, value = 30)
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Data Preview", DTOutput("data_preview")),
                  tabPanel("Top Years", plotOutput("top_years_plot")),
                  tabPanel("BPM Ridge Plot", plotOutput("bpm_ridge")),
                  tabPanel("Top Artists", plotOutput("top_artists")),
                  tabPanel("Streams by Song Age", plotOutput("streams_by_age"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  spotify_data <- reactive({
    req(input$file)
    read_excel(input$file$datapath)
  })
  
  output$data_preview <- renderDT({
    datatable(spotify_data(), options = list(pageLength = 10))
  })
  
  output$top_years_plot <- renderPlot({
    df <- spotify_data() %>%
      mutate(year_released = as.numeric(`Year Released`))
    
    threshold <- quantile(df$Streams, probs = 1 - input$percent_threshold / 100, na.rm = TRUE)
    
    top_years <- df %>%
      filter(Streams >= threshold) %>%
      group_by(year_released) %>%
      summarise(top_songs_count = n()) %>%
      arrange(desc(top_songs_count)) %>%
      head(input$num_years)
    
    ggplot(top_years, aes(x = reorder(as.factor(year_released), top_songs_count), y = top_songs_count)) +
      geom_segment(aes(xend = as.factor(year_released), y = 0, yend = top_songs_count), color = "gray") +
      geom_point(size = 4, color = "darkorange") +
      coord_flip() +
      labs(
        title = paste("Top Years Among the Top", input$percent_threshold, "% Streamed Songs in 2023"),
        x = "Year Released",
        y = "Count of Top-Streamed Songs"
      ) +
      theme_minimal(base_size = 14)
  })
  
  output$bpm_ridge <- renderPlot({
    df <- spotify_data()
    threshold <- quantile(df$Streams, probs = 1 - input$percent_split / 100, na.rm = TRUE)
    
    df <- df %>%
      mutate(BPM = as.numeric(BPM),
             popularity_group = ifelse(Streams >= threshold, paste0("Top ", input$percent_split, "%"), paste0("Bottom ", 100 - input$percent_split, "%"))) %>%
      filter(!is.na(BPM), BPM >= 60, BPM <= 200)
    
    ggplot(df, aes(x = BPM, y = popularity_group, fill = popularity_group)) +
      geom_density_ridges(scale = 1.2, alpha = 0.7, color = "white") +
      labs(
        title = paste("BPM Distribution: Top", input$percent_split, "% vs Rest"),
        x = "BPM (Tempo)", y = ""
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
  
  output$top_artists <- renderPlot({
    df <- spotify_data() %>%
      filter(!is.na(Artist)) %>%
      group_by(Artist) %>%
      summarise(total_streams = sum(Streams, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_streams)) %>%
      slice_max(total_streams, n = input$num_artists)
    
    ggplot(df, aes(x = reorder(Artist, total_streams), y = total_streams)) +
      geom_col(fill = "mediumpurple") +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(
        title = paste("Top", input$num_artists, "Artists by Total Streams (2023)"),
        x = "Artist",
        y = "Total Streams"
      ) +
      theme_minimal(base_size = 14)
  })
  
  output$streams_by_age <- renderPlot({
    df <- spotify_data() %>%
      mutate(
        `Year Released` = as.numeric(`Year Released`),
        SongAge = 2023 - `Year Released`
      ) %>%
      filter(!is.na(SongAge), SongAge <= input$age_bucket)
    
    df <- df %>%
      mutate(age_group = cut(SongAge, breaks = c(0, 5, 10, 15, 20, 25, 30, 40), include.lowest = TRUE))
    
    ggplot(df, aes(x = age_group, y = Streams, fill = age_group)) +
      geom_violin(trim = FALSE, alpha = 0.6) +
      scale_y_continuous(labels = comma) +
      labs(
        title = "Streams by Song Age Group",
        x = "Song Age Group",
        y = "Streams"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
}

shinyApp(ui, server)
