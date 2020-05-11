#Twitter e Text Mining - Solar
#Bruno Freitas

#Instalando as bibliotecas necessarias
# install.packages("rtweet")
# install.packages("wordcloud")
# install.packages("tm")

library(rtweet)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(cluster)   
library(fpc)

#####
#Carregando os Tweets
#Voce precisar ter uma conta no Twitter e autorizar
#Limite de 18.000 tweets a cada 15 minutos -  (No foi passível 4.000)
solar_tweets <- search_tweets(
  "#solar", n = 4000, include_rts = FALSE,lang = "en")

#Rapida visualizaçao - exemplo tirado da propia documentaçao da rtweet
solar_tweets %>%
  ts_plot("1 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequencia de #solar Twitter posts",
    subtitle = "Tweets a cada 1 hora",
    caption = "\nSource: Dados coletados da Twitter's REST API via rtweet"
  )

#####
#O trabalho de Mineraçao de Textos - Text Mining
solar_text <- solar_tweets$text

#Criando e limpando o corpus
solar_text_corpus <- VCorpus(VectorSource(solar_text))
solar_text_corpus <- tm_map(solar_text_corpus,
                              content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))
solar_text_corpus <- tm_map(solar_text_corpus, content_transformer(tolower))
solar_text_corpus <- tm_map(solar_text_corpus, removePunctuation)
solar_text_corpus <- tm_map(solar_text_corpus,removeWords, stopwords("english"))

#Primeira visualização
wordcloud(solar_text_corpus,min.freq=2,max.words=100)
formatacao <- brewer.pal(8,"Dark2")
wordcloud(solar_text_corpus,min.freq=2,max.words=100, random.order=T, colors=formatacao)

#Mas ainda aparece muito lixo

#####
#Limpeza do texto com a Document Term Matrix
solar_dtm <- DocumentTermMatrix(solar_text_corpus)   
solar_dtm

solar_frequencia <- colSums(as.matrix(solar_dtm))   
length(solar_frequencia) 
tail(solar_frequencia,10)

#Removendo termos esparços
solar_dtms <- removeSparseTerms(solar_dtm, 0.98) 
solar_dtms

solar_frequencia <- colSums(as.matrix(solar_dtms))   
length(solar_frequencia) 

solar_frequencia <- sort(colSums(as.matrix(solar_dtms)), decreasing=TRUE) 
solar_frequencia

#Convertendo a matriz de frequencia em dataframe para o plot
solar_plot <- data.frame(word=names(solar_frequencia), freq=solar_frequencia)

#Criando o gráfico
grafico <- ggplot(subset(solar_plot, solar_frequencia>800), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Grafico de barras com os termos mais frequentes") +
  labs(y="Frequencia", x = "Termos")
grafico

#Removendo palavras específicas e limpando novamente o corpus
solar_text_corpus <- tm_map(solar_text_corpus, removeWords, c("energy","solar"))
solar_dtms <- removeSparseTerms(DocumentTermMatrix(solar_text_corpus) , 0.98) 
solar_dtms

solar_frequencia <- colSums(as.matrix(solar_dtms))   
length(solar_frequencia) 

solar_frequencia <- sort(colSums(as.matrix(solar_dtms)), decreasing=TRUE) 
solar_frequencia

#Convertendo a matriz de frequência em dataframe para o plot
solar_plot <- data.frame(word=names(solar_frequencia), freq=solar_frequencia)

#Criando o grafico
grafico <- ggplot(subset(solar_plot, solar_frequencia>800), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle("Grafico de barras com os termos mais frequentes") +
  labs(y="Frequencia", x = "Termos")

#Nova nuvem de palavras
wordcloud(names(solar_frequencia),solar_frequencia,min.freq=2,max.words=150, random.order=T, colors=formatacao)
grafico

#Aplicando um pouco de machine learning - Clustering
solar_dtms2 <- removeSparseTerms(solar_dtms, 0.95)
solar_dtms2

#Clustering 1 - Dendograma
distancia <- dist(t(solar_dtms2), method="euclidian")   
dendograma <- hclust(d=distancia, method="complete")distancia <- dist(t(solar_dtms2), method="euclidian")   
dendograma <- hclust(d=distancia, method="complete")
plot(dendograma, hang=-1,main = "Dendograma Tweets Energia Solar - Bruno Freitas",
     xlab = "Distancia",
     ylab = "Altura")

#Para ler melhor o Dendograma
groups <- cutree(dendograma, k=5)
rect.hclust(dendograma, k=5, border="red")

#Clustering 2 - K-Means
kmeans_btc <- kmeans(distancia, 5)   
clusplot(as.matrix(distancia), kmeans_btc$cluster, color=T, shade=T, labels=3, lines=0,
         main = "K-Means Tweets Enrgia Solar - Outspoken Market",
         xlab = "PC1",
         ylab = "PC2") 
solar_tweets %>%
  ts_plot("1 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequencia de #Energia Solar Twitter posts",
    subtitle = "Tweets a cada 1 hora",
    caption = "\nSource: Dados coletados da Twitter's REST API via rtweet"
  )
