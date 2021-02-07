library(rvest)
library(dplyr)
library(zoo)
library(data.table)

# si el fichero que vamos a obtener ya existe, se borra
if (file.exists("datos.csv")) {
  file.remove("datos.csv")
}

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
        
        cabecera <- readLines(linkFichero, n = 1)
        if (grepl("Grup d'edat", cabecera)) {
          skip <- 0
        } else {
          skip <- 1
        }
        datosDia <- read.csv2(linkFichero, skip = skip)
        
        
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
        
        cabecera <- readLines(linkFichero, n = 1)
        if (grepl("Grup d'edat|Grupo de edad", cabecera)) {
          skip <- 0
        } else {
          skip <- 1
        }
        datosDia <- read.csv2(linkFichero, skip = skip)
        
        
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
        
        cabecera <- readLines(linkFichero, n = 1)
        if (grepl("Grup d'edat|Grupo de edad", cabecera)) {
          skip <- 0
        } else {
          skip <- 1
        }
        datosDia <- read.csv2(linkFichero, skip = skip)
        
        
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
  

# tratamiento de datos. 
#------------------------------------------------------------------------------
#   - Se normalizan los valores de Edad y Sexo
#   - Se calculan variaciones diarias de casos y defunciones
#   - Se calcula la incidencia acumulada a 7 y 14 días para casos y defunciones
datos <- read.csv("datos.csv")
datos <- datos %>% arrange(fecha, Sexo, GrupoEdad) %>% 
  mutate(GrupoEdad = gsub("g90.*", "g90 o mas", GrupoEdad),
         Sexo = gsub("^.*Mujer.*$", "Mujer", Sexo),
         Sexo = gsub("^.*Hombre.*$", "Hombre", Sexo))

# en algunos casos, la fuente no proporciona información para determinadas fechas
# (enlace roto). En estos casos, se interpola la serie de casos acumulados
# y defunciones acumuladas para completar las fechas ausentes.
# En primer lugar, creamos el grid completo de datos:
obsTodas <- expand.grid(
  fecha = as.character(seq.Date(from = as.Date(min(datos$fecha)), 
                                to = as.Date(max(datos$fecha)), 
                                by = "day")
                       ),
  GrupoEdad = c("g0-9",   "g10-19",
                "g20-29", "g30-39",
                "g40-49", "g50-59", 
                "g60-69", "g70-79",
                "g80-89", "g90 o mas"),
  Sexo = c("Hombre", "Mujer")
)
# hacemos el merge entre el grid completo y las observaciones:

datosCompletos <- merge(x = datos, 
                        y = obsTodas,
                        by = c("fecha", "GrupoEdad", "Sexo"), 
                        all.y = TRUE)
# interpolamos CasosAcum y DefAcum
datosCompletos <- datosCompletos %>% 
  arrange(fecha, GrupoEdad, Sexo) %>% 
  group_by(GrupoEdad, Sexo) %>% 
  mutate(CasosAcum = na.approx(CasosAcum, na.rm = FALSE),
         DefAcum   = na.approx(DefAcum,   na.rm = FALSE))

# Se calculan el resto de las variables
datosCompletos <- datosCompletos %>% 
  arrange(fecha, Sexo, GrupoEdad) %>%
  group_by(Sexo, GrupoEdad) %>% 
  mutate(CasosDia = c(0, diff(CasosAcum)),
         DefDia   = c(0, diff(DefAcum)),
         Casos_14d = rollapplyr(CasosDia, width = 14, FUN = sum, fill = 0),
         Def_14d = rollapplyr(CasosDia, width = 14, FUN = sum, fill = 0),
         Casos_7d = rollapplyr(CasosDia, width = 7, FUN = sum, fill = 0),
         Def_7d = rollapplyr(CasosDia, width = 7, FUN = sum, fill = 0),
         PoblacionCV = 5003769,
         PorcCasos = NULL,
         PorcDef   = NULL)


write.csv(datosCompletos, "datos.csv", row.names = FALSE)
