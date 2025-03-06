# Load libraries
library(shiny)
library(dplyr)
library(readr)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(stringr) # For better filtering

# Load the dataset
df_tw <- read_csv("/home/chra.andreou/FinalProject/wordclouds/dataset.csv", 
                  locale = locale(encoding = "UTF-8"),
                  show_col_types = FALSE)

# Keep only the first 300 reviews
df_tw <- df_tw %>% slice(1:300)

# Data transformation
df_sentiment <- df_tw %>%
  mutate(across(everything(), as.character)) %>%
  mutate(review = iconv(review, from = "UTF-8", to = "ASCII//TRANSLIT")) %>% # Clean special characters
  mutate(review = ifelse(is.na(review), "", review)) %>% # Replace NA values with empty strings
  mutate(Sentiment = case_when(
    !is.na(positive) & positive == "1" ~ "Positive",
    !is.na(negative) & negative == "1" ~ "Negative",
    !is.na(neutral) & neutral == "1" ~ "Neutral"
  )) %>%
  filter(!is.na(Sentiment))

# Create the Shiny app UI
ui <- fluidPage(
  titlePanel("Sentiment Analysis Wordcloud"),
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
    mainPanel(
      plotOutput("wordcloud", width = "1000px", height = "800px"),
      textOutput("top_words")  # Text output for top 10 frequent words
    )
  )
)

server <- function(input, output) {
  
  output$wordcloud <- renderPlot({
    # Filter data based on sentiment
    filtered_data <- df_sentiment %>%
      filter(Sentiment == input$sentiment)
    
    # Filter data based on selected topic
    if (input$topic %in% colnames(df_sentiment)) {
      filtered_data <- filtered_data %>% filter(!!sym(input$topic) == "1")
    }
    
    # Combine all reviews into one text string
    reviews_text <- paste(filtered_data$review, collapse = " ")
    
    # Create a corpus (text collection)
    corpus <- Corpus(VectorSource(reviews_text))
    corpus <- tm_map(corpus, content_transformer(tolower)) # Ignore letter case
    corpus <- tm_map(corpus, removePunctuation) # Remove punctuation
    corpus <- tm_map(corpus, removeNumbers) # Remove numbers
    
    # Remove stop words (common words like "the", "and", etc.)
    corpus <- tm_map(corpus, removeWords, stopwords("en")) # Remove stopwords
    
    # Create a word frequency table
    corpus_content <- content(corpus[[1]])
    word_freq <- table(unlist(strsplit(corpus_content, " "))) # Split words and count frequency
    word_freq <- sort(word_freq, decreasing = TRUE) # Sort by frequency in descending order
    
    # Display the top 10 frequent words
    output$top_words <- renderText({
      if(length(word_freq) == 0) {
        return("No words found.")
      }
      top_words <- head(word_freq, 10) # Get top 10 words
      top_words_text <- paste(names(top_words), top_words, sep = ": ", collapse = "\n")
      return(paste("Top 10 words:\n", top_words_text))
    })
    
    # Create the word cloud with larger word sizes
    wordcloud(names(word_freq), 
              freq = word_freq, 
              min.freq = 1, # Allow all words to appear
              scale = c(24, 1),  # Increased scale to make words larger
              colors = brewer.pal(8, "Dark2")) # Use color palette
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
