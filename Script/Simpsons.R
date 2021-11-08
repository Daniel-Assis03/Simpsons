library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(tidytext)
library(reshape2)
library(circlize)
library(textdata)


data_scripts <-read.csv("simpsons_script_lines.csv")
View(data_scripts)




# Descobrindo os 10 Personagens com mais falas e plotando um grafico de barras
data_scripts %>%
  count(raw_character_text)%>%
  arrange((desc(n)))%>%
  slice(1, 3:11)%>%
  ggplot(aes(x=reorder(raw_character_text, n), y=n)) +
  geom_bar(stat="identity", aes(fill=n), show.legend=F) +
  geom_label(aes(label=n)) +
  scale_fill_gradient(low="#35D6ED", high="#97EBF4") +
  labs(x="Personagem", y="N?mero de Falas", title="Personagens com mais falas em Os Simpsons") +
  coord_flip() +
  theme_bw()
  image <- image_read("Homer_Simpson_2006.png")
  grid.raster(image, x=0.91, y=0.31, height=0.4)


  #Personagens com mais falas excluindo a Familia Simpson e plotando um grafico de barras
  data_scripts %>%
    count(raw_character_text)%>%
    arrange((desc(n)))%>%
    slice(6:16)%>%
    ggplot(aes(x=reorder(raw_character_text, n), y=n)) +
    geom_bar(stat="identity", aes(fill=n), show.legend=F) +
    geom_label(aes(label=n)) +
    scale_fill_gradient(low="#58D68D", high="#239B56") +
    labs(x="Personagem", y="N?mero de Falas", title="Personagens com mais falas em Os Simpsons") +
    coord_flip() +
    theme_bw()
    image <- image_read("download.png")
    grid.raster(image, x=0.85, y=0.39, height=0.4)
    


     
##################### ANALISE DE SENTIMENTOS #####################################

#Carregando a biblioteca de sentimentos "bing"
get_sentiments("bing")


#Criando e exibindo os tokens
tokens <- data_scripts%>%
  mutate(dialogues = as.character(data_scripts$normalized_text))%>%
  unnest_tokens(word, dialogues)

tokens %>% 
  head(5) %>% 
 dplyr::select(raw_character_text, word)
  
  
#Plotando uma novem de palavras com as palavras positivas e negativas que mais aparecem
  tokens%>%
    inner_join(bing, "word")%>%
    count(word,sentiment, sort = TRUE)%>%
    acast(word ~ sentiment, value.var  = "n", fill =1)%>%
    comparison.cloud(colors = c("#991D1D", "#327CDE"), max.words = 200)
    image <- image_read("image001.png")
    grid.raster(image, x=0.85, y=0.31, height=0.25)
  
  
 
#Filtrando os dados para obter informaçôes somente da Familia Simpson
plotar <- tokens%>%
  inner_join(bing, "word")%>%
  filter(raw_character_text %in% c("Homer Simpson", "Marge Simpson", "Bart Simpson", "Lisa Simpson"))%>%
  
#Calculando o numero de palavras pelo tipo de sentimento
  count(sentiment, raw_character_text)%>%
  group_by(raw_character_text, sentiment)%>%
  summarise(sentiment_sum = sum(n))%>%
  ungroup()

#Criando o Diagrama de Corda
circos.clear()
circos.par(gap.after = c(rep(2, length(unique(plotar[[1]])) - 1), 15,
                         rep(2, length(unique(plotar[[2]])) - 1), 15), gap.degree=2)
myColors = c("Homer Simpson" = "#FED517", "Marge Simpson" = "#23395d", "Bart Simpson" = "#FF6600", "Lisa Simpson" = "#22BC22",  "positive" = "#D7DBDD", "negative" = "#D7DBDD")
chordDiagram(plotar, grid.col = myColors, transparency = 0.4, annotationTrack = c("name", "grid"),
             annotationTrackHeight = c(0.01, 0.02))
title("Os ?nimos da Familia Simpson")



#Carregando a biblioteca de sentimentos "nrc"
get_sentiments("nrc")

#Aplicando a analise
sentiments <- tokens%>%
  inner_join(nrc, "word")%>%
  count(sentiment, sort = TRUE)
sentiments

#Criando um grafico de barras para mostrar os sentimentos mais presentes
sentiments %>%
  ggplot(aes(x=reorder(sentiment,n), y=n))+
  geom_bar(stat = "identity", aes(fill = sentiment), show.legend = FALSE)+
  geom_label(label = sentiments$n)+
  labs(x =" Sentimentos ", y = "Frequ?ncia", title = "Os Sentimentos Mais Presentes" ) + 
  coord_flip()+
  theme_bw()
  image <- image_read("happyHomer.jpg")
  grid.raster(image, x=0.85, y=0.39, height=0.4)
  

  
  
#Sentimentos mais presentes na Familia Simpson
  
  # Filtrando e aplicando a analise
  sentiments <- tokens%>%
    inner_join(nrc, "word")%>%
    filter(raw_character_text %in% c("Homer Simpson", "Marge Simpson", "Bart Simpson", "Lisa Simpson"))%>%
    count(sentiment, sort = TRUE)
  sentiments
  
  #Criando um grafico de barras para mostrar os sentimentos mais presentes
  sentiments %>%
    ggplot(aes(x=reorder(sentiment,n), y=n))+
    geom_bar(stat = "identity", aes(fill = sentiment), show.legend = FALSE)+
    geom_label(label = sentiments$n)+
    labs(x =" Sentimentos ", y = "Frequ?ncia", title = "Os Sentimentos Mais Presentes na Familia Simpson" ) + 
    coord_flip()+
    theme_minimal()
  image <- image_read("family.jpg")
  grid.raster(image, x=0.82, y=0.39, height=0.4)
  
  
        
    
    
    
    
    
  
