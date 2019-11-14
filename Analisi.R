library(rvest)
library(stringr)
library(openxlsx)
library(stringr) 
library(RCurl)
library(ggplot2)
library(dplyr)
library(httr)
library(XML)
library(tm)
library(topicmodels)
library(factoextra)
library(NbClust)
library(ldatuning)

#setto al directory di lavoro 
setwd("/Users/valentina/Dropbox/ByTek/Clustering")
#apro il file con i risultati dello script "leggi.R" nella stessa cartella
test<-read.xlsx("dati.xlsx")
test<-subset(test, !is.na(test))

#Provo a fare cluster sulle keyword invece che sul testo
###pulisco le keyword
docs <- Corpus(VectorSource(test$Keyword))
#trasformo tutto in minuscolo
docs <- tm_map(docs,content_transformer(tolower))
#rimnuovo simbili potenzialmente probematici
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, " -")
docs <- tm_map(docs, toSpace, "“")
docs <- tm_map(docs, toSpace, "”")
#rimuovo la punteggiatura
docs <- tm_map(docs, removePunctuation)
#rimmuovo i numeri
docs <- tm_map(docs, removeNumbers)
#rimuovo le stopwords
docs <- tm_map(docs, removeWords, stopwords("italian"))
#rimuovo gli spazi extra
docs <- tm_map(docs, stripWhitespace)
#faccio lo stem in italiano
docs <- tm_map(docs,stemDocument,language = "it")
#trasformo i dati in uan matrice pronta per l'analisi
dtm <- DocumentTermMatrix(docs)
m <- as.matrix(dtm)
rownames(m) <- paste(substring(rownames(m),1,3),rep("..",nrow(m)),
                     substring(rownames(m), nchar(rownames(m))-12,nchar(rownames(m))-4))



#Clustering sui testi di google
#Pulisco i testi
sentences <- sub("http://([[:alnum:]|[:punct:]])+", '', test$test)
stopwords_regex = paste(stopwords('it'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
sentences = stringr::str_replace_all(sentences, stopwords_regex, '')

dati2<-sentences
analisi_dati<-Corpus(VectorSource(dati2))
analisi_dati <- tm_map(analisi_dati,content_transformer(tolower))
analisi_dati<-tm_map(analisi_dati,content_transformer(function(x) iconv(x,to='UTF-8-MAC',sub="byte")))
analisi_dati<-tm_map(analisi_dati,content_transformer(tolower))
analisi_dati<-tm_map(analisi_dati,removePunctuation)
analisi_dati<-tm_map(analisi_dati, removeNumbers)
analisi_dati<-tm_map(analisi_dati,function(x)removeWords(x,stopwords(kind="it")))
analisi_dati <-tm_map(analisi_dati, stemDocument, language = "it") # Stemming the words  
#costruisco la matrice per l'analisi
matrix_dati<-TermDocumentMatrix(analisi_dati)
matrDati<-as.matrix(matrix_dati)
#conto i termini più usati
term.freq<-rowSums(matrDati)
word_freq_dati<-sort(rowSums(matrDati),decreasing=TRUE)
dm_Dati<-data.frame(word=names(word_freq_dati),freq=word_freq_dati)

#Costruisco le frequente relative comulate per escludere le parole
#che stanno oltre il 95%
dm_Dati<-dm_Dati[order(dm_Dati$freq),]
dm_Dati$freq_rel<-dm_Dati$freq/sum(dm_Dati$freq)
dm_Dati$freq_cum<-cumsum(dm_Dati$freq_rel)
escludi<-which(dm_Dati$freq_cum>0.95)

###
#Continuo la pulizia dei dati
corpus = tm::Corpus(VectorSource(sentences)) 
corpus <- tm_map(corpus,content_transformer(tolower))
corpus <- tm_map(corpus, stemDocument, language = "it") # Stemming the words  
#aggiungo stopwords da togliere qui c'è da capire perchè non riesc a rimuovere automaticamente le parole più usate
#my_stopwords<- c("quel", "qual","più","può","classf",dm_Dati$word[escludi])
my_stopwords<- c("quel", "qual","più","può","classf","farmac")
#remuovo le custom stopwords
corpus <- tm_map(corpus, removeWords, my_stopwords)

# sistemo eventuali probelmi di  UTF-8 encoding 
corpus <- tm_map(corpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte')) 
# tolgo gli spazi in eccesso
corpus <- tm_map(corpus, stripWhitespace) 


#ricostruisco la matrice di analisi che adesso è pulita
tdm <- DocumentTermMatrix(corpus) 
#normalizzo la matrice
tdm.tfidf <- weightTfIdf(tdm)
#si aggiusta il probelma della matrice sparsa
tdm.tfidf <- removeSparseTerms(tdm.tfidf, 0.98) 
tfidf.matrix <- as.matrix(tdm.tfidf) 
# Definisco la distanza: Cosine distance matrix (useful for specific clustering algorithms) 
dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")

#definisco il numero di cluster per tutte le tecniche uguale al numero di cluster ottimale
#trovato con la metrice Griffiths2004 por LDA con metodo Gibbs


result <- FindTopicsNumber(
  tdm,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = "Griffiths2004",
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
#numero di cluster/topic ottimale
n_topics=result$topics[result$Griffiths2004==max(result$Griffiths2004)]
#trovo i topic con LDA e definisco ogni topic con 6 termini
ldaOut <-LDA(tdm, n_topics, method="Gibbs")
ldaOut.topics <- as.matrix(topics(ldaOut))            
ldaOut.terms <- as.matrix(terms(ldaOut,6))  
test$LDA<-ldaOut.topics
#costruisco una matrice con i topic che poi aggiungerò al dataset iniziale
topicsLDA<-data.frame("LDA"=1:n_topics,"topicLDA"=t(ldaOut.terms))
topicsLDA1<-data.frame("LDA"=1:n_topics, "topicLDA"=apply(topicsLDA[,2:6], 1, function(x) paste(x[!is.na(x) & x != "No"], collapse = ", ")))

#cluster sulle keyword
d <- dist(m)
keyword.hclust <- hclust(d,method="ward.D")
keyword.cutree <- cutree(keyword.hclust, n_topics)
keyword.order <- keyword.hclust$order
#inserisco nel data set i cluster individuati dall'analisi cluster sulle keywords
test$cutree<-keyword.cutree


# faccio clustering con Kmeans sui test
clustering.kmeans <- kmeans(tfidf.matrix, n_topics) 
# faccio clustering sui testi con dbscan (ho messo come numero minimo di osservazioni in ogni cluster 5 ma può essere cambiato)
clustering.dbscan <- dbscan::hdbscan(dist.matrix, minPts = 5)


#creo una colonna Kmeas sul foglio originale di excel con i cluster di kmeans
master.cluster <- clustering.kmeans$cluster 
test$Kmeas<-master.cluster

#creo una colonna DBscan sul foglio originale di excel con i cluster di kmeans
test$DBscan<-clustering.dbscan$cluster


#aggiungo i topic
test2<-merge(test, topicsLDA1, by="LDA", all.x=TRUE)

#salco i risultati in un file excel chiamato test1 nella stessa cartella di lavoro
write.xlsx(test2, "test1.xlsx")

