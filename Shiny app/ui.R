ui <- fluidPage(
  
  load("Shakesdata.Rdata"),
  
  selectInput(inputId = "wordlist", label="Word lists",
              choices=c("Include all words" = "play_words", 
                        "Omit character names" = "play_words_without_characters", 
                        "Omit place names" = "play_words_without_places",
                        "Omit character and place names" = "play_words_without_characters_or_places")
  ),
  
  
  selectizeInput(inputId="sample_plays", label="Plays to compare",
                 choices=shakestexts$play,
                 multiple=TRUE,
                 selected="A Midsummer Night's Dream",
                 options = list(maxItems = 3)
  ),
  
  plotOutput("plot"))