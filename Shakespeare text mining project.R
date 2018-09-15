

#Shakespeare analysis
#The XML files of the plays embed useful semantic information right into the texts, 
#such as where speeches, scenes, and acts start and stop, and which particular 
#characters are entering or exiting a scene, or speaking a speech. Using the encoded 
#texts as a starting point for your project is a significant time-saver in creating 
#mobile apps and other digital projects, or conducting research.
#https://www.folgerdigitaltexts.org/download/xml/FolgerDigitalTexts_XML_Complete.zip

#See Hadley's answer here. https://stackoverflow.com/questions/17198658/how-to-parse-xml-to-r-data-frame
#https://www.folgerdigitaltexts.org/download/xml/FolgerDigitalTexts_XML_Complete.zip


#However, I'm going to start with the raw texts, see if can do that programatically

#https://www.folgerdigitaltexts.org/download/txt/FolgerDigitalTexts_TXT_Complete.zip

install.packages("quanteda")
install.packages("readtext")
install.packages("tidytext")
install.packages("tidyverse")
install.packages("rvest")
library(quanteda)
library(readtext)
library(tidytext)
library(tidyverse)
library(rvest)
library(shiny)

shakestexts <- readtext("https://www.folgerdigitaltexts.org/download/txt/FolgerDigitalTexts_TXT_Complete.zip")

#Okay, to imitate how Julia Silge did it in tidytext, want to arrange the plays
#like how she arranged Austen works So, let's look at her package!
#She arranges them into title and text, very similar

#Let's assign better names to the plays
#I think everything before first line break will be the title
shakestexts[,1] <- str_extract(shakestexts[,2], "[^\n]+")
#And let's rename "doc_id" to "play"
names(shakestexts)[1] <- "play"

play_words <- shakestexts %>%
  unnest_tokens(word, text) %>%
  count(play, word, sort = TRUE) %>%
  ungroup()

total_words <- play_words %>% 
  group_by(play) %>% 
  summarize(total = sum(n))

play_words <- left_join(play_words, total_words)

play_words

play_words <- play_words %>%
  bind_tf_idf(word, play, n)
play_words

play_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

sample_plays <- c("Hamlet", "The Winter's Tale", "A Midsummer Night's Dream",
                  "Henry V", "Romeo and Juliet", "As You Like It")

ggplot(play_words[play_words$play %in% sample_plays,], aes(n/total, fill = play)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~play, ncol = 2, scales = "free_y")

#graph of most distinctive words before removing character names 
play_words[play_words$play %in% shakestexts$play[1:6],] %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(play) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = play)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~play, ncol = 2, scales = "free") +
  coord_flip()


#Let's try removing characters.
charactersource <- "https://www.opensourceshakespeare.org/views/plays/characters/chardisplay.php"
rawcharacterlist <- charactersource %>% read_html %>%
  html_nodes("table") %>% magrittr::extract2(2) %>% 
  html_nodes(xpath="//td//span//b") %>% html_text()

characterlist <- str_split(rawcharacterlist, " ") %>% unlist

#Remove duplicates:
characterlist <- unique(characterlist)

#Remove things that shouldn't be included
stopwords <- c("All", "Character", "Plays", "Both", "Poet", "Butcher", 
               "Bastard", "Younger", "Brother", "Weavers", "Young")
characterlist <-  characterlist[!characterlist %in% stopwords]

#Add a few that should be included, incl play-within-a-play characters,
#aliases, variant spellings, and names of servants who are formally First Servant
characterlist <- c(characterlist, "Baptista", "Gonzago", "Pyramus", "Thisbe",
                   "Barnardo", "Ganymede", "Phoebe", "Aliena", "Rowland",
                   "Rowland's", "Katherine", "Harry", "Lawrence", "Florizel",
                   "Florizell", "Doricles", "Lucianus", "Susan", "Abram", "Tom")

#Make lowercase, or everything fails:
characterlist <- tolower(characterlist)

#Add " 's " to all, since those show up as separate words
characterlist[(length(characterlist)+1):(2*length(characterlist))] <-""

x <- length(characterlist)
for(i in 1:x){
characterlist[x+i] <- paste(characterlist[i], "\'s", sep="", collapse="")
}
characterlist <- unique(characterlist)

characterlist <- as.data.frame(characterlist, stringsAsFactors = FALSE)
names(characterlist)[1] <- "word"


play_words_without_characters <- anti_join(play_words, characterlist, by="word")


play_words_without_characters[play_words_without_characters$play %in% sample_plays,] %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(play) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = play)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~play, ncol = 2, scales = "free") +
  coord_flip()


play_words_without_characters[play_words_without_characters$play %in% "King Lear",] %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(play) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = play)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  #facet_wrap(~play, ncol = 2, scales = "free") +
  coord_flip()

#Okay, that works. How about without place names?

placesource <- "https://en.wikipedia.org/wiki/List_of_Shakespearean_settings"
rawplacelist <- placesource %>% read_html %>% 
  html_nodes(xpath="//li//a[not(i)]") %>% html_text()

#Remove [1], [2], and so on
placelist <- rawplacelist[-grep("\\[[0-9]+\\]", rawplacelist)]

#remove reference section
placelist <- placelist[1:138]
#Remove play names and character names
placelist <-  placelist[!placelist %in% shakestexts$play]
placelist <-  placelist[!placelist %in% characterlist]
#Further cleanup: 
placelist <- placelist[! placelist %in% c("Pericles, the Prince of Tyre" , "Bangor, Wales",
                                          "Richard II (play)", "Henry VI Part 1", "King Henry VI, Part 3",
                                          "Towton/Saxton")]

#Add some places that don't show up in that list
placelist <- c(placelist, "Towton", "Saxton", "Denmark", "England", "Wittenburg",
               "Norway", "Sicilia")
placelist <- tolower(placelist)
placelist <- as.data.frame(placelist, stringsAsFactors = FALSE)
names(placelist)[1] <- "word"

play_words_without_characters_or_places <- anti_join(play_words_without_characters, placelist, by="word")

play_words_without_places <- anti_join(play_words, placelist, by="word")


play_words_without_characters_or_places[play_words_without_characters_or_places$play %in% sample_plays,] %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(play) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = play)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~play, ncol = 2, scales = "free") +
  coord_flip()


#MAKE A SHINY APP!

#Where someone can filter out character names, a further refinement of
#character names, place names
#Being able to sort by words spoken by which character would be great too.
#That would require tagging by line, would take some more work.

#Might use this as inspiration:
#https://public.tableau.com/en-us/s/gallery/word-usage-sacred-texts
#Words per play up top; most distinctive words at def sets
#Easiest implementation: total word count up top, 3 panels of most distinctive
#words, can select whether to exclude char and placenames, can select
#which plays to compare up to 3 or 6

#Use a selectInput to choose wordlist
#Then use a selectizeInput to choose plays in sample plays
#Limit sample plays to 3



save(list=ls(), file="Shakesdata.Rdata")
#About 5 MB, should be fine.

#Templates from Wordchomp:

shinyApp(
ui <- fluidPage(

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
      
      plotOutput("plot")),



server <- function(input, output){
  
  #libraries
  library(tidyverse)

  #Acquire data
  load("Shakesdata.Rdata")
  
  #Define make_graph
  make_graph <- function(wordlist, sample_plays) {
    graph <- filter(wordlist, play %in% sample_plays) %>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word)))) %>% 
      group_by(play) %>% 
      top_n(15) %>% 
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = play)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~play, ncol = 3, scales = "free") +
      coord_flip()
    
    return(graph)
  }
  
  #Produce output
  output$plot <- renderPlot({make_graph(eval(as.name(input$wordlist)), 
                                        input$sample_plays)})
}
)

