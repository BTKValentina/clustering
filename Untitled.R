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

setwd("/Users/valentina/Dropbox/ByTek/Clustering")
#funzione che fa scraping dei risltati di google

get_google_serp_urls <- function(querie, number_of_results, country_code, language_code, user_agent){
  
  serp_url <- paste0("https://www.google.com/search?q=",querie,"&num=",number_of_results,"&cr=country",country_code,"&lr=lang_",language_code)
  
  serp_url <- str_replace_all(serp_url,"\\s+","+")
  serp_url <- as.character(serp_url)
  
  request <- GET(serp_url, user_agent(user_agent))
  
  #doc <- htmlParse(request, encoding = "UTF-8")
  #doc <- htmlParse(request, asText = TRUE, encoding = "UTF-8")
  return(request)
  
}


#carico il db son le keyword
dati<-read.xlsx("Keyword.xlsx")
dati$testo<-NA

#inserisco la parola da cercare, il numero di rusltati, il contry e la lingua

number_of_results <- 10
country_code <- "IT"
language_code <- "it"
user_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.99 Safari/537.36"
dati$test<-NA
for (i in 124:dim(dati)[1]){
  querie <- dati$Keyword[i]
doc <- get_google_serp_urls(querie, number_of_results, country_code, language_code, user_agent)

a<-doc %>% 
  # as.character() %>%   # in case strings are stored as factors
  read_html() %>% 
  html_nodes('[class="st"]') 

ciccio<-paste(a, collapse=', ' )
pippo<-gsub('span class=\"st\"',"", ciccio)
pippo<-gsub('span',"", pippo)
pippo<-gsub('em',"", pippo)

pippo<- gsub('[[:digit:]]+', '', pippo)
pippo <- gsub('[[:punct:]]+', '', pippo)

dati$test[i]<-pippo}




test<-subset(dati, !is.na(test))
sentences <- sub("http://([[:alnum:]|[:punct:]])+", '', test$test)
stopwords_regex = paste(stopwords('it'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
sentences = stringr::str_replace_all(sentences, stopwords_regex, '')

corpus = tm::Corpus(tm::VectorSource(sentences)) 

# Cleaning up 
# Handling UTF-8 encoding problem from the dataset 
corpus.cleaned <- tm::tm_map(corpus, function(x) iconv(x, to='UTF-8-MAC', sub='byte'))  
#corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::removeWords, tm::stopwords('italian')) # Removing stop-words 

corpus.cleaned <- tm::tm_map(corpus, tm::stemDocument, language = "it") # Stemming the words  
corpus.cleaned <- tm::tm_map(corpus.cleaned, tm::stripWhitespace) # Trimming excessive whitespaces

#rappresentazione numerica del testo
tdm <- tm::DocumentTermMatrix(corpus.cleaned) 
tdm.tfidf <- tm::weightTfIdf(tdm)

#si aggiusta il probelma della matrice sparsa
tdm.tfidf <- tm::removeSparseTerms(tdm.tfidf, 0.999) 
tfidf.matrix <- as.matrix(tdm.tfidf) 
# Cosine distance matrix (useful for specific clustering algorithms) 
dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")

#definisco il numero di cluster uguale al numero di cluster individualto maualmente (qui
#da implementare metodo automatico per defnire il nuemero ottimale di cluster)
nk=length(unique(test$Cluster.1))

clustering.kmeans <- kmeans(tfidf.matrix, nk) 
clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
clustering.dbscan <- dbscan::hdbscan(dist.matrix, minPts = 10)

#Cerco i clustering migliore

master.cluster <- clustering.kmeans$cluster 
slave.hierarchical <- cutree(clustering.hierarchical, k = nk) 
slave.dbscan <- clustering.dbscan$cluster 
stacked.clustering <- rep(NA, length(master.cluster))  
names(stacked.clustering) <- 1:length(master.cluster) 
for (cluster in unique(master.cluster)) { 
  indexes = which(master.cluster == cluster, arr.ind = TRUE) 
  slave1.votes <- table(slave.hierarchical[indexes]) 
  slave1.maxcount <- names(slave1.votes)[which.max(slave1.votes)]   
  slave1.indexes = which(slave.hierarchical == slave1.maxcount, arr.ind = TRUE) 
  slave2.votes <- table(slave.dbscan[indexes]) 
  slave2.maxcount <- names(slave2.votes)[which.max(slave2.votes)]   
  stacked.clustering[indexes] <- slave2.maxcount 
}


points <- cmdscale(dist.matrix, k = 2) 
palette <- colorspace::diverge_hcl(nk) # Creating a color palette 
previous.par <- par(mfrow=c(2,2), mar = rep(1.5, 4)) 

plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Hierarchical clustering', col = as.factor(slave.hierarchical), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Stacked clustering', col = as.factor(stacked.clustering), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
par(previous.par) # recovering the original plot space parameters

#creo una colonna Kmeas sul foglio originale di excel con i cluster di kmeans
test$Kmeas<-master.cluster
#provo LDA

ldaOut <-LDA(tdm,nk, method="Gibbs")
ldaOut.topics <- as.matrix(topics(ldaOut))            
ldaOut.terms <- as.matrix(terms(ldaOut,6))  
test$LDA<-ldaOut.topics
topicsLDA<-data.frame("LDA"=1:nk,"topicLDA"=t(ldaOut.terms))
test1<-merge(test, topicsLDA, by="LDA", all.x=TRUE)



