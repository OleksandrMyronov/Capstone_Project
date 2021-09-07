library(shiny)
library(sbo)
library(dplyr)
library(tidyr)
library(tidytext)
library(igraph)
library(ggraph)
NgramBase<-readRDS("./trigram10_model.rds")   #loading database of N-grams for sbo algorithm
p<-sbo_predictor(NgramBase)                   #initializing sbo algorithm
CorrBase<-readRDS("./correlation_model.rds")  #loading database of word correlations
  
# Define server logic
shinyServer(function(input, output) {

# Reactive statement for prediction function
    prediction =  reactive( {
        inputText = input$text
        tibble(inputText) %>%
          unnest_tokens(word, inputText) ->WordList
        
        CorrBase %>%
          filter(item1 %in% WordList$word) %>%
          group_by(item2) %>%
          summarize(correlation=sum(correlation)) %>%
          arrange(desc(correlation)) %>%
          head(10) -> corr_output        #correlation prediction
        
        sbo_output=head(predict(p, inputText),10) #Sbo prediction
        prediction=cbind(SBO=sbo_output, Correlation=corr_output$item2)
    })    
    
# Output    
    output$table = renderTable(prediction(), colnames=T)

# Plotting graph of sbo predicted words    
    output$wordChain = renderPlot({
      
      inputText = input$text
      tibble(inputText) %>%
        unnest_tokens(word, inputText, token='ngrams', n=1) %>%
        tail(input$sliderLength)-> textWords
      N<-length(textWords$word)   # number of words in word chain
      predictWords<-NULL
      currentPrediction<-NULL
      for (i in 1:N) {
        currentPrediction<-predict(p, paste(textWords$word[1:i], collapse=" "))[1:input$sliderN]
        for (j in 1:length(currentPrediction)) {
          predictWords<-rbind(predictWords, 
                              data.frame(word1=textWords$word[i], 
                                         word2=currentPrediction[j],
                                         wordType="predicted"
                              ))
        }
        
        if (i<N) {
          predictWords<-filter(predictWords, (word2!=textWords$word[i+1])|(word1!=textWords$word[i]))
          predictWords<-rbind(predictWords, 
                              data.frame(word1=textWords$word[i], 
                                         word2=textWords$word[i+1],
                                         wordType="real")) 
        }
      } 
      
      set.seed(2021)
      a <- grid::arrow(type = "closed", angle=18, length = unit(.2, "inches"))
      
        ggraph(graph_from_data_frame(predictWords), layout = "fr") +
        geom_edge_link(show.legend = F,
                       arrow = a, end_cap = circle(.07, 'inches'),
                       aes(color=predictWords$wordType
                       ), width=1.2) +
        geom_node_point(color = "lightblue", size = 5) +   
        geom_node_text(aes(label = name), repel=T, size=6)+ 
        theme_void()
    }) #
})
