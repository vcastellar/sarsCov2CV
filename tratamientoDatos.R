# tratamiento de datos. 
#------------------------------------------------------------------------------


#   - Se normalizan los valores de Edad y Sexo
#   - Se calculan variaciones diarias de casos y defunciones
#   - Se calcula la incidencia acumulada a 7 y 14 días para casos y defunciones
if (file.exists("./data/datosEdad.csv")) {
  file.remove("./data/datosEdad.csv")
}
datos <- read.csv("./data/datosRAW.csv", stringsAsFactors = FALSE)
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

datosCompletosEdad <- merge(x = datos, 
                        y = obsTodas,
                        by = c("fecha", "GrupoEdad", "Sexo"), 
                        all.y = TRUE)
# interpolamos CasosAcum y DefAcum
datosCompletosEdad <- datosCompletosEdad %>% 
  arrange(fecha) %>% 
  group_by(GrupoEdad, Sexo) %>% 
  mutate(CasosAcum = na.approx(CasosAcum, na.rm = FALSE),
         DefAcum   = na.approx(DefAcum,   na.rm = FALSE))

# Se calculan el resto de las variables
datosCompletosEdad <- datosCompletosEdad %>% 
  arrange(fecha, Sexo, GrupoEdad) %>%
  group_by(Sexo, GrupoEdad) %>% 
  mutate(CasosDia = c(0, diff(CasosAcum)),
         DefDia   = c(0, diff(DefAcum)),
         Casos_14d = rollapplyr(CasosDia, width = 14, FUN = sum, fill = 0),
         Def_14d = rollapplyr(DefDia, width = 14, FUN = sum, fill = 0),
         Casos_7d = rollapplyr(CasosDia, width = 7, FUN = sum, fill = 0),
         Def_7d = rollapplyr(DefDia, width = 7, FUN = sum, fill = 0),
         PoblacionCV = 5003769)


write.csv(datosCompletosEdad, "./data/datosEdad.csv", row.names = FALSE)
