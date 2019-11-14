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
library(factoextra)


#si setta la directory su cui si lavora
setwd("/Users/valentina/Dropbox/ByTek/Clustering")

#funzione che fa scraping dei risulutati di google
get_google_serp_urls <- function(querie, number_of_results, country_code, language_code, user_agent){
  
  serp_url <- paste0("https://www.google.com/search?q=",querie,"&num=",number_of_results,"&cr=country",country_code,"&lr=lang_",language_code)
  
  serp_url <- str_replace_all(serp_url,"\\s+","+")
  serp_url <- as.character(serp_url)
  #
  request <- GET(serp_url, user_agent(user_agent))
  
  #doc <- htmlParse(request, encoding = "UTF-8")
  #doc <- htmlParse(request, asText = TRUE, encoding = "UTF-8")
  return(request)
  
}
#lista dei file nella directory
list.files()
#carico il db dove son le keyword
keyword="keyword.xlsx"
dati<-read.xlsx(keyword)
dati$testo<-NA

# il numero di risultati, il country e la lingua

number_of_results <- 5
country_code <- "IT"
language_code <- "it"
user_agent <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_13_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/67.0.3396.99 Safari/537.36"
dati$test<-NA


for (i in 1:dim(dati)[1]){
  #pesco la keyword da cercare nel file che ho caricato, il nome della colonna dove sta la keyword deve essere "Keyword"
  querie <- dati$Keyword[i]
  doc <- get_google_serp_urls(querie, number_of_results, country_code, language_code, user_agent)
  
  a<-doc %>% 
    # as.character() %>%   # in case strings are stored as factors
    read_html() %>% 
    html_nodes('[class="st"]')  #perchè il title sta qui
  #Sys.sleep(6)
  #metto tutti i title in un'unica cella
  ciccio<-paste(a, collapse=', ' )
  #tolgo un po' di schifezze
  pippo<-gsub('span class=\"st\"',"", ciccio)
  pippo<-gsub('span',"", pippo)
  pippo<-gsub('em',"", pippo)
  pippo<- gsub('[[:digit:]]+', '', pippo)
  pippo <- gsub('[[:punct:]]+', '', pippo)
  dati$test[i]<-pippo}

#Salvo i dati in un file chiamato dati.xlsx nella stessa cartella di lavoro 
write.xlsx(dati, "dati.xlsx")
