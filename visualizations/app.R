library(shiny)
library(dplyr)
library(tidyr)
library(readr)

# Φόρτωση και καθαρισμός του dataset
df_clean <- read_csv("/home/chra.andreou/FinalProject/visualizations/dataset.csv", locale = locale(encoding = "UTF-8"))

df_clean <- df_clean %>%
  mutate(across(everything(), ~ iconv(as.character(.), from = "UTF-8", to = "UTF-8", sub = "NA"))) %>%
  select(id, review, Clean, Comfort, `Facilities/Amenities`, Location, 
         `Restaurant (dinner)`, Staff, `View (Balcony)`, Breakfast, Room, 
         Pool, Beach, `Bathroom/Shower (toilet)`, Bar, Bed, Parking, Noise, 
         `Reception-checkin`, `Lift`, `Value for money`, `Wi-Fi`, positive, negative, neutral)

# UI του Shiny
ui <- fluidPage(
  titlePanel("Interactive Review Sentiment and Topics Viewer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("review_select", "Select Review ID:", choices = df_clean$id),
      textOutput("review_text"),
      textOutput("sentiment_output"),
      textOutput("topics_output")
    ),
    mainPanel(
      verbatimTextOutput("review_info")
    )
  )
)

# Server του Shiny
server <- function(input, output) {
  
  # Ανάκτηση του επιλεγμένου review
  selected_review <- reactive({
    df_clean %>% filter(id == input$review_select)
  })
  
  # Εμφάνιση του κειμένου του review
  output$review_text <- renderText({
    selected_review()$review
  })
  
  # Καθορισμός του sentiment
  output$sentiment_output <- renderText({
    sentiment <- "Neutral"  # Default αν δεν υπάρχει άλλος τρόπος
    
    # Ελέγχουμε τις στήλες "positive", "negative" και "neutral"
    if (!is.na(selected_review()$positive) && selected_review()$positive == 1) {
      sentiment <- "Positive"
    } else if (!is.na(selected_review()$negative) && selected_review()$negative == 1) {
      sentiment <- "Negative"
    } else if (!is.na(selected_review()$neutral) && selected_review()$neutral == 1) {
      sentiment <- "Neutral"
    }
    
    paste("Sentiment: ", sentiment)
  })
  
  # Εμφάνιση των κατηγοριών (topics) που σχετίζονται με το review
  output$topics_output <- renderText({
    # Ελέγχουμε όλες τις στήλες για το review
    topics <- names(selected_review())[grepl("Clean|Comfort|Facilities|Location|Restaurant|Staff|View|Breakfast|Room|Pool|Beach|Bathroom|Bar|Bed|Parking|Noise|Reception|Lift|Wi-Fi", names(selected_review()))]
    
    # Επιλέγουμε τις κατηγορίες που έχουν τιμή 1
    selected_topics <- topics[selected_review()[, topics] == 1]
    paste("Topics: ", paste(selected_topics, collapse = ", "))
  })
  
  # Προβολή επιπλέον πληροφοριών για το review (όλες οι κατηγορίες και το συναίσθημα)
  output$review_info <- renderPrint({
    list(
      review_text = selected_review()$review,
      sentiment = ifelse(!is.na(selected_review()$positive) && selected_review()$positive == 1, "Positive", 
                         ifelse(!is.na(selected_review()$negative) && selected_review()$negative == 1, "Negative", 
                                ifelse(!is.na(selected_review()$neutral) && selected_review()$neutral == 1, "Neutral", "Unknown"))),
      topics = names(selected_review())[grepl("Clean|Comfort|Facilities|Location|Restaurant|Staff|View|Breakfast|Room|Pool|Beach|Bathroom|Bar|Bed|Parking|Noise|Reception|Lift|Wi-Fi", names(selected_review()))][selected_review()[, grepl("Clean|Comfort|Facilities|Location|Restaurant|Staff|View|Breakfast|Room|Pool|Beach|Bathroom|Bar|Bed|Parking|Noise|Reception|Lift|Wi-Fi", names(selected_review()))] == 1]
    )
  })
}

# Εκτέλεση του Shiny app
shinyApp(ui = ui, server = server)
