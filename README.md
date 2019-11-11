# clustering

lo Script R fa le seguenti cose:
  - prende un file excel con delle keyword
  - per ogni keyword si scarica i primi 10 risultati di google organici
  - unisce i risultati in un unico testo
  - pulisce il testo: toglie a la punteggiatura, le stopwrds (è impostato in italiano ma si può cambiare), fa lo stemming delle parole in italiano.
  - clusterizza le parole analizzando il testo associato in base a :
    -  kMeans (con numero di cluster uguale al numero di cluster trovati manualmente)
    - Dbscan (nessun risultato DA RIVEDERE)
    - LDA
  - associa un cluster a seconda dell'algoritmo utilizzato ad ogni parola. 
