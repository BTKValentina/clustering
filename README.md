# clustering

lo Script Untitled.R fa le seguenti cose:
  - prende un file excel con delle keyword
  - per ogni keyword si scarica i primi 10 risultati di google organici
  - unisce i risultati in un unico testo
  - pulisce il testo: toglie a la punteggiatura, le stopwrds (è impostato in italiano ma si può cambiare), fa lo stemming delle parole in italiano.
  - clusterizza le parole analizzando il testo associato in base a :
    -  kMeans (con numero di cluster uguale al numero di cluster trovati manualmente)
    - Dbscan (nessun risultato DA RIVEDERE)
    - LDA
  - associa un cluster a seconda dell'algoritmo utilizzato ad ogni parola. 
  
  
  #Cosa c'è nei file excel Keyword
  Il file excel Keyword.xlsx è il file di partenza e contiene le seguenti colonne:
 
  - keyword madre: keyword usata per espandere la ricerca
  - keyword: keyword trovata su keyword planner
  - Cluster 1: classificazione manuale 
 
  
  Il file excel Test.xlsx è il file di arrivo e contiene le seguenti colonne:
  - LDA cluster secondo l'algoritmo LDA 
  - keyword madre: keyword usata per espandere la ricerca
  - keyword: keyword trovata su keyword planner
  - Cluster 1: classificazione manuale 
  - Cutree: classificazione gerarchica sulle keyword e non sui testi
  - Kmeas: clister secondo l'algortimo kmeans
  - topicLDA 1 - topicLDA6: i sei termini che compongono ciascun topic
  
  
  
  
  
