# Εισαγωγή βιβλιοθηκών
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyverse)

# Ανάγνωση δεδομένων
df_tw <- read_csv("/home/chra.andreou/FinalProject/sentimentanalysis/dataset.csv", 
                  locale = locale(encoding = "UTF-8"))

# Καθαρισμός δεδομένων
df_clean <- df_tw %>%
  mutate(across(everything(), ~ iconv(as.character(.), from = "UTF-8", to = "UTF-8", sub = "NA"))) %>%
  select(id, review, positive, negative, neutral, Clean, Comfort, `Facilities/Amenities`, Location, 
         `Restaurant (dinner)`, Staff, `View (Balcony)`, Breakfast, Room, Pool, Beach, 
         `Bathroom/Shower (toilet)`, Bar, Bed, Parking, Noise, `Reception-checkin`, `Lift`, 
         `Value for money`, `Wi-Fi`)

# Υποθέτουμε ότι df_clean είναι ήδη φορτωμένο και έτοιμο προς χρήση
df_sentiment <- df_clean %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = c("Clean", "Comfort", "Facilities/Amenities", "Location", "Restaurant (dinner)", "Staff", 
                        "View (Balcony)", "Breakfast", "Room", "Pool", "Beach", "Bathroom/Shower (toilet)", 
                        "Bar", "Bed", "Parking", "Noise", "Reception-checkin", "Lift", "Value for money", "Wi-Fi"), 
               names_to = "Aspect", values_to = "Score") %>%
  filter(Score == 1) %>%
  select(id, review, positive, negative, neutral, Aspect)

# Υπολογισμός του sentiment για κάθε review
df_sentiment <- df_sentiment %>%
  mutate(Sentiment = case_when(
    !is.na(positive) & positive == 1 ~ "Positive",
    !is.na(negative) & negative == 1 ~ "Negative",
    !is.na(neutral) & neutral == 1 ~ "Neutral"
  )) %>%
  filter(!is.na(Sentiment))  # Φιλτράρουμε τα NA στο Sentiment

# UI του Shiny
ui <- fluidPage(
  titlePanel("Sentiment Analysis for Selected Aspect"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("aspect_select", "Select Aspect:", choices = unique(df_sentiment$Aspect)),
      selectInput("chart_type", "Select Chart Type:", choices = c("Bar Chart", "Density Plot")),
      textOutput("aspect_title")
    ),
    
    mainPanel(
      plotlyOutput("sentiment_plot"),
      tableOutput("sentiment_table")
    )
  )
)

# Server του Shiny
server <- function(input, output) {
  
  # Ανάκτηση των δεδομένων για το επιλεγμένο aspect
  selected_data <- reactive({
    df_selected <- df_sentiment %>%
      filter(Aspect == input$aspect_select) %>%
      filter(!is.na(Sentiment))  # Διασφαλίζουμε ότι το Sentiment δεν είναι NA
    return(df_selected)
  })
  
  # Δημιουργία του γράφηματος με βάση τον τύπο
  output$sentiment_plot <- renderPlotly({
    aspect_data <- selected_data()
    
    # Αν δεν υπάρχουν δεδομένα για το επιλεγμένο aspect, επιστρέφουμε NULL
    if (nrow(aspect_data) == 0) {
      return(NULL)
    }
    
    # Επιλογή του τύπου του γραφήματος
    if (input$chart_type == "Bar Chart") {
      plot <- ggplot(aspect_data, aes(x = Sentiment, fill = Sentiment)) +
        geom_bar(position = "fill") +
        labs(title = paste("Sentiment Distribution for", input$aspect_select),
             x = "Sentiment",
             y = "Proportion",
             fill = "Sentiment") +
        theme_minimal()
    } else if (input$chart_type == "Density Plot") {
      plot <- ggplot(aspect_data, aes(x = Sentiment, fill = Sentiment)) +
        geom_density(alpha = 0.5) +
        labs(title = paste("Sentiment Density for", input$aspect_select),
             x = "Sentiment",
             y = "Density") +
        theme_minimal()
    }
    
    ggplotly(plot)
  })
  
  # Εμφάνιση τίτλου για το επιλεγμένο aspect
  output$aspect_title <- renderText({
    paste("Displaying sentiment distribution for:", input$aspect_select)
  })
  
  # Εμφάνιση πίνακα με τα δεδομένα του επιλεγμένου aspect
  output$sentiment_table <- renderTable({
    selected_data() %>%
      group_by(Sentiment) %>%
      summarise(count = n())
  })
}

# Εκτέλεση του Shiny app
shinyApp(ui = ui, server = server)
