server <- function(input, output){
  
  #libraries
  library(tidyverse)
  
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
