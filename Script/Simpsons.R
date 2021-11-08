#Carregando as Bibliotecas
library(dplyr)
library(ggplot2)
library(stats)
library(magick)
library(grid)
library(tm)
library(treemap)
library(treemapify)
library(wordcloud)
library(tidytext)
library(reshape2)
library(circlize)
library(radarchart)
library(tidyr)
library(memisc)


#Definindo  o diretorio de trabalho
setwd("C:/Users/User/Desktop/Estudos/Analises/Simpsons Analise 2.0/")
getwd()

#Carregando os dados e visualizando (View)
data_scripts <-read.csv("C:/Users/User/Desktop/Estudos/Analises/Simpsons Analise 2.0/Dataset/simpsons_script_lines.csv")
View(data_scripts)

#Bibliotecas de Analise de Sentimentos
bing <- read.csv("C:/Users/User/Desktop/Estudos/Analises/Simpsons Analise 2.0/SentimentAnalises/Bing.csv")
nrc <- read.csv("C:/Users/User/Desktop/Estudos/Analises/Simpsons Analise 2.0/SentimentAnalises/NRC.csv")
afinn <- read.csv("C:/Users/User/Desktop/Estudos/Analises/Simpsons Analise 2.0/SentimentAnalises/Afinn.csv")


get_sentiments("bing")

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
  image <- image_read("C:/Users/User/Desktop/Simpsons Analise 2.0/Images/Homer_Simpson_2006.png")
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
    image <- image_read("C:/Users/User/Desktop/Simpsons Analise 2.0/Images/download.png")
    grid.raster(image, x=0.85, y=0.39, height=0.4)
    

#Verificando e criando uma vizualiza??o dos locais que mais aparecem no seriado

pal<-c ("#35D6ED", "#58D68D")
data_scripts%>%
      count(raw_location_text)%>%
      arrange((desc(n)))%>%
      slice(1:10)%>%
      treemap(index = "raw_location_text",
              vSize = "n", palette = pal, 
              type = "index",
              title = "Locais com mais Apari??es")
     
##################### ANALISE DE SENTIMENTOS#####################################

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
    image <- image_read("C:/Users/User/Desktop/Simpsons Analise 2.0/Images/image001.png")
    grid.raster(image, x=0.85, y=0.31, height=0.25)
  
  
 
#Filtrando os dados para obter informa??es somente da Familia Simpson
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
#image <- image_read("C:/Users/User/Desktop/Simpsons Analise 2.0/Images/crazyhomer.jpg")
#grid.raster(image, x=0.875, y=0.18, height=0.45)


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
  image <- image_read("C:/Users/User/Desktop/Simpsons Analise 2.0/Images/happyHomer.jpg")
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
  image <- image_read("C:/Users/User/Desktop/Simpsons Analise 2.0/Images/family.jpg")
  grid.raster(image, x=0.82, y=0.39, height=0.4)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 # char_sentiments <- tokens%>%
  #  inner_join(nrc, "word")%>%
   # filter(raw_character_text %in% c("Homer Simpson", "Marge Simpson", "Bart Simpson", "Lisa Simpson")
    #      & !sentiment %in% c("positive", "negative"))%>%
    #group_by(raw_character_text, sentiment)%>%
    #count(raw_character_text, sentiment)%>%
    #dplyr::select(raw_character_text, sentiment, char_sentiments_count = n)
  
  
  
#  total_char <- tokens %>%
 #   inner_join(nrc,"word")%>%
  #  filter(raw_character_text %in% c("Homer Simpson", "Marge Simpson", "Bart Simpson", "Lisa Simpson")
   #        & !sentiment %in% c("positive", "negative"))%>%
  #  group_by(raw_character_text, sentiment)%>%
   # count(raw_character_text)%>%
  #  dplyr::select(raw_character_text, total = n)
  
  
#  char_sentiments %>% 
 #   inner_join(total_char, by="raw_character_text") %>% 
  #  mutate(percent = char_sentiments_count / total * 100 ) %>% 
   # dplyr::select(char_sentiments_count, -total) %>% 
    #spread(raw_character_text, percent) %>% 
    #chartJSRadar(showToolTipLabel = T, main="Character and Sentiment Radar", maxScale=22, responsive=T,
     #            addDots = T, 
      #           colMatrix = grDevices::col2rgb(c("#FA8072","#04700A","#062D82","#99F1EB","#F39C12")),
       #          lineAlpha = 0.7, polyAlpha = 0.05)
  
        
    
    
    
    
    
  