library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(countrycode)

df <- read_csv("../data/raw/Students-Social-Media-Addiction.csv")

custom_css <- "
h2, .panel-title {
    color: #0F1F3D !important;
}

body {
    background-color: #F4F6F9 !important;
}

.card-header {
    background-color: #c8d2df !important;
    color: #0F1F3D !important;
    font-weight: bold;
}

.card {
    border: none !important;
    box-shadow: 0 2px 5px rgba(0,0,0,0.1) !important;
}

.bslib-value-box {
    border: none !important;
    border-left: 5px solid #c8d2df !important;
    box-shadow: 0 2px 5px rgba(0,0,0,0.1) !important;
    color: #0F1F3D !important;
}

.shiny-text-output {
    color: #0F1F3D !important;
}
"

ui <- page_fluid(
  tags$head(
    tags$style(custom_css)
  ),

  titlePanel("Social Media Addiction Dashboard"),


  layout_sidebar(
    sidebar = sidebar(
      h6("Filters"),
      radioButtons(
        "f_gender",
        "Gender",
        choices = c("All", "Male", "Female"),
        selected = "All",
        inline = FALSE
      ),
      selectInput(
        "f_level",
        "Academic Level",
        choices = c("All", "Undergraduate", "Graduate"),
        selected = "All"
      ),

      open = "desktop",
      bg = "#EEF1F6",
      fg = "#0F1F3D"
    ),

    layout_columns(
      value_box(
        title = "Total Students",
        value = textOutput("tile_students"),
      ),
      value_box(
        title = "Avg Daily Usage",
        value = textOutput("tile_usage"),
      ),
      value_box(
        title = "Avg Sleep Hours",
        value = textOutput("tile_sleep"),
      ),
      value_box(
        title = "Avg Addiction Score",
        value = textOutput("tile_addiction"),
      ),
      fill = FALSE
    ),

    layout_columns(
      card(
        card_header("Addiction vs Mental Health & Sleep"),
        plotlyOutput("scatter_chart"),
        full_screen = TRUE
      )
    )
  )
)


server <- function(input, output, session) {
  custom_ui_colors <- c('#0F1F3D', '#2D6BE4', '#26f7fd')

  filtered_df <- reactive({
    data <- df |>
      filter(Academic_Level %in% c("Undergraduate", "Graduate"))

    if (input$f_gender != "All") {
      data <- data |> filter(Gender == input$f_gender)
    }

    if (input$f_level != "All") {
      data <- data |> filter(Academic_Level == input$f_level)
    }


    data
  })

  output$tile_students <- renderText({
    nrow(filtered_df())
  })

  output$tile_usage <- renderText({
    d <- filtered_df()
    if (nrow(d) == 0) return("--")
    paste0(round(mean(d$Avg_Daily_Usage_Hours, na.rm = TRUE), 1), "h")
  })

  output$tile_sleep <- renderText({
    d <- filtered_df()
    if (nrow(d) == 0) return("--")
    paste0(round(mean(d$Sleep_Hours_Per_Night, na.rm = TRUE), 1), "h")
  })

  output$tile_addiction <- renderText({
    d <- filtered_df()
    if (nrow(d) == 0) return("--")
    round(mean(d$Addicted_Score, na.rm = TRUE), 1)
  })

  output$scatter_chart <- renderPlotly({
    d <- filtered_df()
    if (nrow(d) == 0) return(NULL)
    
    d <- d |>
      mutate(
        jitter_addiction = Addicted_Score + runif(n(), -0.4, 0.4),
        jitter_mental = Mental_Health_Score + runif(n(), -0.4, 0.4)
      )

    p <- ggplot(d, aes(x = jitter_addiction, y = jitter_mental, 
                       color = Sleep_Hours_Per_Night,
                       text = paste("Addiction Score:", Addicted_Score,
                                    "<br>Mental Health Score:", Mental_Health_Score,
                                    "<br>Sleep Time (hrs):", Sleep_Hours_Per_Night))) +
      geom_point(alpha = 0.7, size = 2) +
      scale_color_gradientn(colors = custom_ui_colors, name = "Sleep Time (hrs)") +
      labs(x = "Addiction Score", y = "Mental Health Score")

    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)
