library(tidyverse)
library(tidytext)
library(magrittr)
library(lubridate)
library(stringr)
library(ggExtra)
library(ggplot2)
library(wordcloud2)
library(gridExtra)
library(plotly)
library(RColorBrewer)
library(widyr)
library(igraph)
library(ggraph)
#library(sentiment)
library(lexiconPT)
library(devtools)
#if(!require(Rstem)) install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
#if(!require(sentiment)) install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
library(Rstem)
library(sentiment)
library(wordcloud)
library(twitteR)
library(dplyr)
library(ggplot2)
library(stringr)

#chaves de acesso da API

api_key             = "XXXXXXXXXXXXXX"
api_secret          = "XXXXXXXXXXXXXX"
access_token        = "XXXXXXXXXXXXXX"
access_token_secret = "XXXXXXXXXXXXXX"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#pesquisa no twitter
pesq <- searchTwitter("volta aulas", n=10000)

pesq_tweets <- twListToDF(pesq)

pesq_tweets$text <- as.character(pesq_tweets$text)
# remove retweet
pesq_tweets$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "",pesq_tweets$text)
# remove usuario
pesq_tweets$text <- gsub("@\\w+", "",pesq_tweets$text)
# remove pontuacao
pesq_tweets$text <- gsub('[[:punct:]]', ' ', pesq_tweets$text)
# remove numeros
pesq_tweets$text <- gsub('[[:digit:]]', ' ', pesq_tweets$text)
# remove html e links
pesq_tweets$text <- gsub('http\\w+', ' ', pesq_tweets$text)
# remove espacos
pesq_tweets$text <- gsub('[ \t]{2,}', ' ', pesq_tweets$text)
pesq_tweets$text <- gsub('^\\s+|\\s+$', ' ', pesq_tweets$text)
# remove emojis e caracteres
pesq_tweets$text <- gsub('<.*>', '', enc2native(pesq_tweets$text))
pesq_tweets$text <- gsub("[~|^|~|\"|'|`]"," ",pesq_tweets$text)
# caixa baixa
pesq_tweets$text <- tolower(pesq_tweets$text)
pesq_tweets$text <- iconv(pesq_tweets$text,to="ASCII//TRANSLIT")

#funcao para limpar os tweets
f_limpa_tweets <- function (tweets) {
  limpa_tweets <- sapply(tweets, function(x) x$getText())
  # remove retweet
  limpa_tweets <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', ' ', limpa_tweets)
  # remove usuario
  limpa_tweets <- gsub('@\\w+', ' ', limpa_tweets)
  # remove pontuacao
  limpa_tweets <- gsub('[[:punct:]]', ' ', limpa_tweets)
  # remove numeros
  limpa_tweets <- gsub('[[:digit:]]', ' ', limpa_tweets)
  # remove html e links
  limpa_tweets <- gsub('http\\w+', ' ', limpa_tweets)
  # remove espacos
  limpa_tweets <- gsub('[ \t]{2,}', ' ', limpa_tweets)
  limpa_tweets <- gsub('^\\s+|\\s+$', ' ', limpa_tweets)
  # remove emojis e caracteres
  limpa_tweets <- gsub('<.*>', '', enc2native(limpa_tweets))
  limpa_tweets <- gsub("[~|^|~|\"|'|`]"," ",limpa_tweets)
  # caixa baixa
  limpa_tweets <- tolower(limpa_tweets)
  
  limpa_tweets
}

#funcao para remover os acentos
f_remove_acento <- function(texto){
  limpo <- iconv(texto, to="ASCII//TRANSLIT") 
  limpo
}

#chama funcoes de limpeza
#pesq_tweets$tex <- f_limpa_tweets(pesq_tweets$text)
#pesq_tweets$tex <- f_remove_acento(pesq_tweets$text)
#pesq_tweets
#class(pesq_tweets)

# remove retweets duplicados
pesq_tweets <- pesq_tweets[!duplicated(pesq_tweets$text),]

pesq_tweets
class(pesq_tweets)

#exportando dados
#write.table(x= pesq_tweets, file = "C:/Users/fermat/Desktop/Desktop/MESTRADO ENSINO/TecnoligiasEducacionais/twitter.txt")

#obtendo apenas o campo texto
tweets_texto <- pesq_tweets$text

#classificador de emocoes
emocoes <- classify_emotion(tweets_texto, algorithm='bayes')

#classificador de polaridades
polaridades = classify_polarity(tweets_texto, algorithm='bayes')

#criar dataframe das emocoes e polaridades por tweets
df = data.frame(text=tweets_texto, emotion=emocoes[,'BEST_FIT'],
                polarity=polaridades[,'BEST_FIT'], stringsAsFactors=FALSE)
df[is.na(df)] <- "N.A."

aggregate(data.frame(count = df$emotion), list(value = df$emotion), length)

#plot emocoes
plot_ly(df, x=~emotion,type="histogram",
        marker = list(color = c('grey', 'red',
                                'orange', 'navy',
                                'yellow'))) %>%
  layout(yaxis = list(title='Valores'), xaxis = list(title='Emoções'), title="Análise de Sentimentos: (volta aulas)")

#plot polaridades
plot_ly(df, x=~polarity, type="histogram",
        marker = list(color = c('magenta', 'gold',
                                'lightblue'))) %>%
  layout(yaxis = list(title='Valores'), xaxis = list(title='Polaridades'), title="Análise de Sentimentos: (volta aulas)")

#plot emocoes tweets
tweet_pies <- ggplot(as.data.frame(polaridades),aes(x = factor(1), fill = factor(BEST_FIT)))+geom_bar(width = 1)
tweet_pies + coord_polar(theta = "y")+
  ggtitle("AnÃ¡lise de Sentimentos",subtitle = "Volta as aulas")+
  ylab("Y")+xlab("X")+scale_fill_brewer(palette = "RdYlGn")+
  theme(plot.title = element_text(size = 12,face = "bold"))

#tweets por polaridade
df <- df %>%
  group_by(polarity) %>%
  summarise(pasted=paste(text, collapse=" "))

# removendo stopwords
df$pasted = removeWords(df$pasted, stopwords(kind='pt'))

# criar corpus
corpus = Corpus(VectorSource(df$pasted))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = df$polarity
# gerando word cloud
comparison.cloud(tdm, colors = brewer.pal(3, 'Dark2'), random.order = F, title.size = 1.5)

palavras<-sort(rowSums(tdm),decreasing = T)
testeCloud <- data.frame(word=names(palavras),freq=palavras)

wordcloud(words = testeCloud$word, freq = testeCloud$freq, min.freq = 1,
          max.words = 200, random.order = F, rot.per = 0.35,
          colors = brewer.pal(8,"Dark2"))

#LexiconPT
data("oplexicon_v3.0")
data("sentiLex_lem_PT02")

op30 <- oplexicon_v3.0
sent <- sentiLex_lem_PT02

str(op30)
table(op30$type)
table(op30$polarity)
table(op30$polarity_revision)

#criar novo dataframe e criar id para cada tweet
class(pesq_tweets)
pesq_tweets %<>% mutate(tweet_id = row_number())
#cria uma linha para cada palavra do tweet
df_tweet_palavra <- pesq_tweets %>% unnest_tokens(term,text)

#testar correlações de palavras
correlacao <- df_tweet_palavra %>%
  group_by(term) %>%
  filter(n()>20) %>%
  pairwise_cor(term, tweet_id, sort=TRUE)
correlacao
corplot(correlacao)
#testando a rede de correlação
correlacao %>%
  filter(correlation > 0.4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'fr') +
  guides(edge_alpha = "none", edge_width = "nome")+
  scale_edge_color_gradientn(limits = c(-1,1),colors = c("firebrick2", "dodgerblue2"))+
  geom_edge_link(aes(edge_alpha = correlation),show.legend = FALSE)+
  geom_node_point(color = "lightblue", size=5)+
  geom_node_text(aes(label=name),repel = TRUE)+
  theme_graph() +
  labs(title = "Correlação de Palavras: (volta aulas)")

#verificando a polaridade das palavras
df_tweet_palavra %>%
  left_join(op30, by = "term") %>%
  left_join(sent %>% select(term, lex_polarity = polarity), by = "term")%>%
  select(tweet_id,term, polarity, lex_polarity) %>%
  head(50)

#palavrar que tenham em ambos os dicionarios
df_tweet_palavra <- df_tweet_palavra %>%
  inner_join(op30, by = "term") %>%
  inner_join(sent %>% select(term, lex_polarity = polarity), by = "term")%>%
  group_by(tweet_id) %>%
  summarise(
    tweet_sentiment_op = sum(polarity),
    tweet_sentiment_lex = sum(lex_polarity),
    n_palavras = n())%>%
  ungroup() %>% rowwise() %>%
  mutate(
    most_neg = min(tweet_sentiment_lex, tweet_sentiment_op),
    most_pos = max(tweet_sentiment_lex, tweet_sentiment_op)
  )


head(df_tweet_palavra)

#gerando gráfico de polaridade entre os dois léxicos
df_tweet_palavra %>%
  ggplot(aes(tweet_sentiment_op,tweet_sentiment_lex))+
  geom_point(aes(color = n_palavras))+
  scale_color_continuous(low="green",high="red")+
  labs(x="Polaridades OpLexicon",y="Polaridades Sentilex",title = "Gráfico dos Termos: volta aulas")+
  geom_smooth(method = "lm")+
  geom_vline(xintercept = 0,linetype="dashed")+
  geom_hline(yintercept = 0,linetype="dashed")


df_tweet_palavra %<>% filter(between(tweet_sentiment_op,-10,10))

#comentario mais positivo e negativo dos tweets
most_pos <- which.max(df_tweet_palavra$most_pos)
most_neg <- which.min(df_tweet_palavra$most_neg)

#mais positivo
cat(pesq_tweets$text[pesq_tweets$tweet_id == df_tweet_palavra$tweet_id[most_pos]])
#mais negativo
cat(pesq_tweets$text[pesq_tweets$tweet_id == df_tweet_palavra$tweet_id[most_neg]])

#utilizando a analise de sentimento Op Lexico
pesq_tweets %<>% inner_join(
  df_tweet_palavra %>% select(tweet_id, sentiment = tweet_sentiment_op),
  by = "tweet_id")

#criar coluna data
pesq_tweets$data <- as.Date(pesq_tweets$created)

df_pesq_tweets <- pesq_tweets %>%
  #filtrar fora palavras neutras
  filter(sentiment != 0) %>%
  #converter numerico para categorico
  mutate(sentiment = ifelse(sentiment <0,"negativo","positivo"))%>%
  #agrupar os dados
  count(data, substr(text,1,40),sentiment)%>%
  #converter para formato wide
  spread(sentiment,n,fill = 0)%>%
  mutate(sentimento = positivo - negativo) %>%
  ungroup() %>% arrange(data)

head(df_pesq_tweets,50) %>% knitr::kable()

#classificando mais positivos e negativos
df_pesq_tweets %>%
  arrange(sentimento) %>% filter(row_number()==1 | row_number()==nrow(df_pesq_tweets))%>%
  knitr::kable()

aggregate(data.frame(count = df_pesq_tweets$sentimento),
          list(value = df_pesq_tweets$sentimento), length)

#evoluçao do sentimento dos tweets ao longo do tempo
df_pesq_tweets %>%
  mutate(index = row_number())%>%
  ggplot(aes(x=index,y=sentimento))+
  geom_col(aes(fill=sentimento))+
  scale_y_continuous(breaks = seq(-3,4,1),limits = c(-3,4))+
  labs(x="Indice da publicação",y="Sentimento",fill = "Pesos",tittle = "Evolução do sentimentos")


