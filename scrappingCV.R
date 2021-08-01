library(rvest)
library(dplyr)
library(zoo)
library(data.table)

# si el fichero que vamos a obtener ya existe, se borra
if (file.exists("./data/datosRAWEdad.csv")) {
  file.remove("./data/datosRAWEdad.csv")
}

urls <- c("https://dadesobertes.gva.es/dataset/covid-19-dades-de-casos-i-persones-mortes-per-grup-edat-i-sexe-acumulades-des-del-31-01-2020",
          "https://dadesobertes.gva.es/va/dataset/covid19-casos-i-persones-mortes-per-grup-edat-i-sexe-2020")

#------------------------------------------------------------------------------
# extracción de datos por grupos de edad y sexo
#------------------------------------------------------------------------------

url <- urls[1]
  html <- read_html(url)
  
  links <- html %>%
    html_nodes("a") %>%
    html_attr("href") 
  links <- links[grepl("/dataset/covid-19-dades", links)]
  links <- unique(links[grepl("/resource/", links)])
  
  
  raiz <- "https://dadesobertes.gva.es"
  contador <- 0
  for (link in links) {
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
        
        cabecera <- readLines(linkFichero, n = 1)
        if (grepl("Grup d'edat", cabecera)) {
          skip <- 0
        } else {
          skip <- 1
        }
        datosDia <- read.csv2(linkFichero, skip = skip)
        
        
        datosDia$fecha <- fecha
        names(datosDia) <- c("GrupoEdad", "Sexo", "PorcCasos", "CasosAcum", "PorcDef", "DefAcum","fecha", "fechaExtrac")
        write.table(file = "./data/datosRAWEdad.csv", datosDia, 
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

  links <- unique(links[grepl("/va/dataset/.*resource", links)])
  

  for (link in links) {
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
        
        cabecera <- readLines(linkFichero, n = 1)
        if (grepl("Grup d'edat|Grupo de edad", cabecera)) {
          skip <- 0
        } else {
          skip <- 1
        }
        datosDia <- read.csv2(linkFichero, skip = skip)
        
        
        datosDia$fecha <- as.numeric(gsub("-", "", fecha))
        names(datosDia) <- c("GrupoEdad", "Sexo", "PorcCasos", "CasosAcum", "PorcDef", "DefAcum", "fecha")
        
        write.table(file = "./data/datosRAWEdad.csv", datosDia, 
                    row.names = FALSE, append = TRUE, 
                    col.names = ifelse(contador < 1, TRUE, FALSE),
                    sep = ",",
                    dec = ".")
        contador <- contador + 1
      }
    },silent = TRUE)
    
  }
  
  pobXGEdad <- data.frame(GrupoEdad = c("g0-9","g10-19", "g20-29", "g30-39", "g40-49",
                                           "g50-59", "g60-69", "g70-79", "g80-89", "g90 o mas"),
                          PobbEdad  = c(460070,534393,508767,632014,841917,749547,579666,440802,230837,50634))
  save(pobXGEdad, file = "./data/pobXGEdad.RData")                                       
  