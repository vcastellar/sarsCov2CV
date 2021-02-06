library(rvest)
library(dplyr)

urls <- c("https://dadesobertes.gva.es/dataset/covid-19-dades-de-casos-i-persones-mortes-per-grup-edat-i-sexe-acumulades-des-del-31-01-2020",
          "https://dadesobertes.gva.es/va/dataset/covid19-casos-i-persones-mortes-per-grup-edat-i-sexe-2020")

url <- urls[1]
  html <- read_html(url)
  
  links <- html %>%
    html_nodes("a") %>%
    html_attr("href") 
  links <- links[grepl("/dataset/covid-19-dades", links)]
  links <- links[grepl("/resource/", links)]
  
  
  raiz <- "https://dadesobertes.gva.es"
  contador <- 0
  for (link in unique(links)) {
    try({
      fichero <- read_html(paste0(raiz, link))
      fecha <- fichero %>% 
        html_nodes("title") %>%
        html_text() 
      if (length(fecha) > 0) {
        fecha <- gsub(" - Generalitat Valenciana", "", fecha)
        fecha <- fecha %>% 
          strsplit(NULL) %>% 
          lapply(rev) %>% 
          sapply(paste, collapse="") %>% 
          substr(1, 10) %>%
          strsplit(NULL) %>% 
          lapply(rev) %>% 
          sapply(paste, collapse="")
        
        linkFichero <- fichero %>% 
          html_nodes("a") %>% 
          html_attr("href") 
        linkFichero <- linkFichero[grepl("download", linkFichero)][1]
        
        datosDia <- read.csv2(linkFichero, skip = 1)
        
        
        datosDia$fecha <- fecha
        names(datosDia) <- c("GrupoEdad", "Sexo", "PorcCasos", "CasosAcum", "PorcDef", "DefAcum", "fecha")
        write.table(file = "datos.csv", datosDia, 
                    row.names = FALSE, append = TRUE, 
                    col.names = ifelse(contador < 1, TRUE, FALSE),
                    sep = ",",
                    dec = ".")
        contador <- contador + 1
      }
    },silent = TRUE)
    
  }

url <- urls[2]
  html <- read_html(url)
  
  links <- html %>%
    html_nodes("a") %>%
    html_attr("href") 
  # links <- links[grepl("/download/.*casosyfallecidos_porcentajessexosedad\\.csv$", links)]
  links <- links[grepl("/va/dataset/.*resource", links)]
  

  for (link in unique(links)) {
    try({
      fichero <- read_html(paste0(raiz, link))
      fecha <- fichero %>% 
        html_nodes("title") %>%
        html_text() 
      if (length(fecha) > 0) {
        fecha <- gsub(" - Generalitat Valenciana", "", fecha)
        fecha <- fecha %>% 
          strsplit(NULL) %>% 
          lapply(rev) %>% 
          sapply(paste, collapse="") %>% 
          substr(1, 10) %>%
          strsplit(NULL) %>% 
          lapply(rev) %>% 
          sapply(paste, collapse="")
        
        linkFichero <- fichero %>% 
          html_nodes("a") %>% 
          html_attr("href") 
        linkFichero <- linkFichero[grepl("download", linkFichero)][1]
        
        datosDia <- read.csv2(linkFichero)
        
        
        datosDia$fecha <- fecha
        names(datosDia) <- c("GrupoEdad", "Sexo", "PorcCasos", "CasosAcum", "PorcDef", "DefAcum", "fecha")
        write.table(file = "datos.csv", datosDia, 
                    row.names = FALSE, append = TRUE, 
                    col.names = ifelse(contador < 1, TRUE, FALSE),
                    sep = ",",
                    dec = ".")
        contador <- contador + 1
      }
    },silent = TRUE)
    
  }
  
  
  
  url <- urls[2]
  html <- read_html(url)
  
  links <- html %>%
    html_nodes("a") %>%
    html_attr("href") 
  # links <- links[grepl("/download/.*casosyfallecidos_porcentajessexosedad\\.csv$", links)]
  links <- links[grepl("/ca/dataset/.*resource", links)]
  
  
  for (link in unique(links)) {
    try({
      fichero <- read_html(gsub("\\/ca\\/", "\\/va\\/",paste0(raiz, link)))
      fecha <- fichero %>% 
        html_nodes("title") %>%
        html_text() 
      if (length(fecha) > 0) {
        fecha <- gsub(" - Generalitat Valenciana", "", fecha)
        fecha <- fecha %>% 
          strsplit(NULL) %>% 
          lapply(rev) %>% 
          sapply(paste, collapse="") %>% 
          substr(1, 10) %>%
          strsplit(NULL) %>% 
          lapply(rev) %>% 
          sapply(paste, collapse="")
        
        linkFichero <- fichero %>% 
          html_nodes("a") %>% 
          html_attr("href") 
        linkFichero <- linkFichero[grepl("download", linkFichero)][1]
        
        datosDia <- read.csv2(linkFichero)
        
        
        datosDia$fecha <- fecha
        names(datosDia) <- c("GrupoEdad", "Sexo", "PorcCasos", "CasosAcum", "PorcDef", "DefAcum", "fecha")
        write.table(file = "datos.csv", datosDia, 
                    row.names = FALSE, append = TRUE, 
                    col.names = ifelse(contador < 1, TRUE, FALSE),
                    sep = ",",
                    dec = ".")
        contador <- contador + 1
      }
    },silent = TRUE)
    
  }
  


datos <- read.csv("datos.csv")
datos <- datos %>% arrange(fecha)

write.csv(datos, "datos.csv", row.names = FALSE)
