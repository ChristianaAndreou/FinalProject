# Φόρτωση βιβλιοθηκών
library(shiny)
library(dplyr)
library(readr)
library(wordcloud)
library(tm)
library(RColorBrewer)  # Για χρώματα στο wordcloud

# Φόρτωση του dataset
df_tw <- read_csv("/home/chra.andreou/FinalProject/sentimentanalysis/dataset.csv", locale = locale(encoding = "UTF-8"))

# Καθαρισμός και μετασχηματισμός των δεδομένων
df_sentiment <- df_tw %>%
  mutate(across(everything(), as.character)) %>%
  mutate(Sentiment = case_when(
    !is.na(positive) & positive == 1 ~ "Positive",
    !is.na(negative) & negative == 1 ~ "Negative",
    !is.na(neutral) & neutral == 1 ~ "Neutral"
  )) %>%
  filter(!is.na(Sentiment))  # Αφαίρεση γραμμών με NA στην κατηγορία Sentiment

# Δημιουργία της εφαρμογής shiny
ui <- fluidPage(
  
  # Τίτλος
  titlePanel("Sentiment Analysis Wordcloud"),
  
  # Ενότητες: Επιλογή Topic και Sentiment
  sidebarLayout(
    sidebarPanel(
      selectInput("topic", "Select Topic:",
                  choices = c("Clean", "Comfort", "Facilities/Amenities", "Location", 
                              "Restaurant (dinner)", "Staff", "View (Balcony)", 
                              "Breakfast", "Room", "Pool", "Beach", 
                              "Bathroom/Shower (toilet)", "Bar", "Bed", 
                              "Parking", "Noise", "Reception-checkin", 
                              "Lift", "Value for money", "Wi-Fi")),
      
      selectInput("sentiment", "Select Sentiment:",
                  choices = c("Positive", "Negative", "Neutral"))
    ),
    
    # Περιεχόμενο (Wordcloud)
    mainPanel(
      plotOutput("wordcloud")
    )
  )
)

server <- function(input, output) {
  
  output$wordcloud <- renderPlot({
    
    # Φιλτράρισμα των δεδομένων με βάση το topic και το sentiment
    filtered_data <- df_sentiment %>%
      filter(Sentiment == input$sentiment) %>%
      select(input$topic) %>%
      na.omit()  # Αφαίρεση των NA τιμών
    
    # Αν το filtered_data είναι άδειο (δηλαδή δεν υπάρχουν δεδομένα για τον συνδυασμό topic και sentiment), επιστρέφει μια κενή απεικόνιση
    if (nrow(filtered_data) == 0) {
      plot.new()  # Δημιουργεί μια κενή περιοχή σχεδίασης
      text(0.5, 0.5, "No reviews available for this combination.", cex = 1.5)
      return()
    }
    
    # Συνένωση όλων των κριτικών σε ένα κείμενο
    reviews_text <- paste(filtered_data[[1]], collapse = " ")
    
    # Δημιουργία corpus και καθαρισμός κειμένου
    corpus <- Corpus(VectorSource(reviews_text))
    corpus <- tm_map(corpus, content_transformer(tolower)) # Μετατροπή σε πεζά
    corpus <- tm_map(corpus, removePunctuation)            # Αφαίρεση σημάτων στίξης
    corpus <- tm_map(corpus, removeNumbers)                # Αφαίρεση αριθμών
    corpus <- tm_map(corpus, removeWords, stopwords("en"))  # Αφαίρεση κοινών λέξεων (stop words)
    corpus <- tm_map(corpus, stripWhitespace)              # Αφαίρεση κενών
    
    # Δημιουργία πίνακα συχνοτήτων λέξεων
    word_freq <- table(unlist(strsplit(corpus[[1]]$content, " ")))
    word_freq <- sort(word_freq, decreasing = TRUE)
    
    # Αν οι λέξεις είναι λιγότερες από το ελάχιστο όριο, επιστρέφει μια κενή απεικόνιση
    if (length(word_freq) == 0) {
      plot.new()  # Δημιουργεί μια κενή περιοχή σχεδίασης
      text(0.5, 0.5, "Not enough words to generate a word cloud.", cex = 1.5)
      return()
    }
    
    # Δημιουργία του wordcloud
    wordcloud(names(word_freq), 
              freq = word_freq, 
              min.freq = 1,  # Μειώστε το min.freq αν θέλετε να εμφανιστούν περισσότερες λέξεις
              scale = c(3, 0.5), # Μέγεθος λέξεων
              colors = brewer.pal(8, "Dark2")) # Χρώματα χωρίς χρήση του cex
  })
}

# Εκκίνηση της εφαρμογής
shinyApp(ui = ui, server = server)
