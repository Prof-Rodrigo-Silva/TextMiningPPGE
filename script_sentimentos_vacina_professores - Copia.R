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
pesq1 <- searchTwitter("vacina professores",n=10000)
class(pesq1)
pesq_tweets1 <- twListToDF(pesq1)

pesq_tweets1$text <- as.character(pesq_tweets1$text)
# remove retweet
pesq_tweets1$text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "",pesq_tweets1$text)
# remove usuario
pesq_tweets1$text <- gsub("@\\w+", "",pesq_tweets1$text)
# remove pontuacao
pesq_tweets1$text <- gsub('[[:punct:]]', ' ', pesq_tweets1$text)
# remove numeros
pesq_tweets1$text <- gsub('[[:digit:]]', ' ', pesq_tweets1$text)
# remove html e links
pesq_tweets1$text <- gsub('http\\w+', ' ', pesq_tweets1$text)
# remove espacos
pesq_tweets1$text <- gsub('[ \t]{2,}', ' ', pesq_tweets1$text)
pesq_tweets1$text <- gsub('^\\s+|\\s+$', ' ', pesq_tweets1$text)
# remove emojis e caracteres
pesq_tweets1$text <- gsub('<.*>', '', enc2native(pesq_tweets1$text))
pesq_tweets1$text <- gsub("[~|^|~|\"|'|`]"," ",pesq_tweets1$text)
# caixa baixa
pesq_tweets1$text <- tolower(pesq_tweets1$text)
pesq_tweets1$text <- iconv(pesq_tweets1$text,to="ASCII//TRANSLIT")

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
pesq_tweets1 <- pesq_tweets1[!duplicated(pesq_tweets1$text),]

pesq_tweets1
class(pesq_tweets1)

#exportando dados
#write.table(x= pesq_tweets, file = "C:/Users/fermat/Desktop/Desktop/MESTRADO ENSINO/TecnoligiasEducacionais/twitter.txt")

#obtendo apenas o campo texto
tweets_texto1 <- pesq_tweets1$text

#tweets_texto1 <- removeWords(tweets_texto1, stopwords(kind='pt'))

#classificador de emocoes
emocoes1 <- classify_emotion(tweets_texto1, algorithm='bayes')

#classificador de polaridades
polaridades1 = classify_polarity(tweets_texto1, algorithm='bayes')

#criar dataframe das emocoes e polaridades por tweets
df1 = data.frame(text=tweets_texto1, emotion=emocoes1[,'BEST_FIT'],
                polarity=polaridades1[,'BEST_FIT'], stringsAsFactors=FALSE)
df1[is.na(df1)] <- "N.A."

aggregate(data.frame(count = df1$emotion), list(value = df1$emotion), length)

#plot emocoes
plot_ly(df1, x=~emotion,type="histogram",
        marker = list(color = c('grey', 'red',
                                'orange', 'navy',
                                'yellow'))) %>%
  layout(yaxis = list(title='Valores'), xaxis = list(title='Emoções'), title="Análise de Sentimentos: (vacina professores)")

#plot polaridades
plot_ly(df1, x=~polarity, type="histogram",
        marker = list(color = c('magenta', 'gold',
                                'lightblue'))) %>%
  layout(yaxis = list(title='Valores'), xaxis = list(title='Polaridades'), title="Análise de Sentimentos: (vacina professores)")

#plot emocoes tweets
tweet_pies1 <- ggplot(as.data.frame(emocoes1),aes(x = factor(1), fill = factor(BEST_FIT)))+geom_bar(width = 1)
tweet_pies1 + coord_polar(theta = "y")+
  ggtitle("An??lise de Sentimentos",subtitle = "Vacinação de Professores")+
  ylab("Y")+xlab("X")+scale_fill_brewer(palette = "RdYlGn")+
  theme(plot.title = element_text(size = 12,face = "bold"))

#tweets por polaridade
df1 <- df1 %>%
  group_by(polarity) %>%
  summarise(pasted=paste(text, collapse=" "))

# removendo stopwords
df1$pasted = removeWords(df1$pasted, stopwords(kind='pt'))

# criar corpus
corpus1 = Corpus(VectorSource(df1$pasted))
tdm1 = TermDocumentMatrix(corpus1)
tdm1 = as.matrix(tdm1)
colnames(tdm1) = df1$polarity

# gerando word cloud
comparison.cloud(tdm1, colors = brewer.pal(3, 'Dark2'), random.order = F, title.size = 1.5)

palavras1<-sort(rowSums(tdm1),decreasing = T)
testeCloud1 <- data.frame(word=names(palavras1),freq=palavras1)

wordcloud(words = testeCloud1$word, freq = testeCloud1$freq, min.freq = 1,
          max.words = 200, random.order = F, rot.per = 0.35,
          colors = brewer.pal(8,"Dark2"))
#LexiconPT
data("oplexicon_v3.0")
data("sentiLex_lem_PT02")

op301 <- oplexicon_v3.0
sent1 <- sentiLex_lem_PT02

str(op301)
table(op301$type)
table(op301$polarity)
table(op301$polarity_revision)

#criar novo dataframe e criar id para cada tweet
class(pesq_tweets1)
pesq_tweets1 %<>% mutate(tweet_id = row_number())
#cria uma linha para cada palavra do tweet
df_tweet_palavra1 <- pesq_tweets1 %>% unnest_tokens(term,text)

#testar correla????es de palavras
correlacao1 <- df_tweet_palavra1 %>%
  group_by(term) %>%
  filter(n()>10) %>%
  pairwise_cor(term, tweet_id, sort=TRUE)
correlacao1

#testando a rede de correla????o
correlacao1 %>%
  filter(correlation > 0.4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'fr') +
  guides(edge_alpha = "none", edge_width = "nome")+
  scale_edge_color_gradientn(limits = c(-1,1),colors = c("firebrick2", "dodgerblue2"))+
  geom_edge_link(aes(edge_alpha = correlation),show.legend = FALSE)+
  geom_node_point(color = "lightblue", size=5)+
  geom_node_text(aes(label=name),repel = TRUE)+
  theme_graph() +
  labs(title = "Correlação de Palavras: (vacina professores)")

#verificando a polaridade das palavras
df_tweet_palavra1 %>%
  left_join(op301, by = "term") %>%
  left_join(sent1 %>% select(term, lex_polarity = polarity), by = "term")%>%
  select(tweet_id,term, polarity, lex_polarity) %>%
  head(50)

#palavrar que tenham em ambos os dicionarios
df_tweet_palavra1 <- df_tweet_palavra1 %>%
  inner_join(op301, by = "term") %>%
  inner_join(sent1 %>% select(term, lex_polarity = polarity), by = "term")%>%
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


head(df_tweet_palavra1)

#gerando gr??fico de polaridade entre os dois l??xicos
df_tweet_palavra1 %>%
  ggplot(aes(tweet_sentiment_op,tweet_sentiment_lex))+
  geom_point(aes(color = n_palavras))+
  scale_color_continuous(low="green",high="red")+
  labs(x="Polaridades OpLexicon",y="Polaridades Sentilex", title = "Gráfico dos Termos: vacina professores")+
  geom_smooth(method = "lm")+
  geom_vline(xintercept = 0,linetype="dashed")+
  geom_hline(yintercept = 0,linetype="dashed")


df_tweet_palavra1 %<>% filter(between(tweet_sentiment_op,-10,10))

#comentario mais positivo e negativo dos tweets
most_pos1 <- which.max(df_tweet_palavra1$most_pos)
most_neg1 <- which.min(df_tweet_palavra1$most_neg)

#mais positivo
cat(pesq_tweets1$text[pesq_tweets1$tweet_id == df_tweet_palavra1$tweet_id[most_pos1]])
#mais negativo
cat(pesq_tweets1$text[pesq_tweets1$tweet_id == df_tweet_palavra1$tweet_id[most_neg1]])

#utilizando a analise de sentimento Op Lexico
pesq_tweets1 %<>% inner_join(
  df_tweet_palavra1 %>% select(tweet_id, sentiment = tweet_sentiment_op),
  by = "tweet_id")

#criar coluna data
pesq_tweets1$data <- as.Date(pesq_tweets1$created)

df_pesq_tweets1 <- pesq_tweets1 %>%
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

head(df_pesq_tweets1,50) %>% knitr::kable()

#classificando mais positivos e negativos
df_pesq_tweets1 %>%
  arrange(sentimento) %>% filter(row_number()==1 | row_number()==nrow(df_pesq_tweets1))%>%
  knitr::kable()

#evolu??ao do sentimento dos tweets ao longo do tempo
df_pesq_tweets1 %>%
  mutate(index = row_number())%>%
  ggplot(aes(x=index,y=sentimento))+
  geom_col(aes(fill=sentimento))+
  scale_y_continuous(breaks = seq(-3,4,1),limits = c(-2,4))+
  labs(x="Indice da publicação",y="Sentimento",
       fill = "Peso",
       tittle = "Evolução do sentimentos")
