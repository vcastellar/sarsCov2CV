# generación de gráficos de incidencia y defunciones en CV

library(dplyr)
library(ggplot2)
library(plotly)

source("customTheme.R")

# importar datos fuente gva
datos <- read.csv("datos.csv")



# datos por grupo de edad
datosEdad <- datos %>% group_by(fecha, GrupoEdad) %>% 
  summarise(CasosAcum = sum(CasosAcum, na.rm = TRUE),
            CasosDia  = sum(CasosDia, na.rm = TRUE),
            DefAcum   = sum(DefAcum, na.rm = TRUE),
            DefDia    = sum(DefDia, na.rm = TRUE),
            Casos_7d  = sum(Casos_7d, na.rm = TRUE),
            Casos_14d = sum(Casos_14d, na.rm = TRUE),
            Def_7d    = sum(Def_7d, na.rm = TRUE),
            Def_14d   = sum(Def_14d, na.rm = TRUE)
            )

p <- ggplot(datosEdad, aes(as.Date(fecha), CasosDia, color = GrupoEdad)) + geom_bar(stat = "identity") +
  facet_wrap(. ~ GrupoEdad) + 
  labs(title = "nº casos diarios por grupos de edad en CV",
       subtitle = paste("fecha de generación: ", Sys.Date()),
       caption = "Fuente de Datos: https://dadesobertes.gva.es") +
  xlab("Fecha") + ylab("Casos Diarios") +
  scale_x_date(date_breaks = "1 month") +
  custom_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                       legend.position = "none",
                       panel.grid.major.x = element_blank())
png(filename = "./plots/casosXGruposEdad.png", width = 1800, height = 900)
p
dev.off()


p <- ggplot(datosEdad, aes(as.Date(fecha), DefDia, color = GrupoEdad)) + geom_bar(stat = "identity") +
  facet_wrap(. ~ GrupoEdad) + 
  labs(title = "nº defunciones diarias por grupos de edad en CV",
       subtitle = paste("fecha de generación: ", Sys.Date()),
       caption = "Fuente de Datos: https://dadesobertes.gva.es") +
  xlab("Fecha") + ylab("Casos Diarios") +
  ylab("Defunciones Diarias") +
  xlab("Fecha") +
  scale_x_date(date_breaks = "1 month") +
  custom_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                       legend.position = "none",
                       panel.grid.major.x = element_blank())
png(filename = "./plots/defuncionesXGruposEdad.png", width = 1800, height = 900)
p
dev.off()

#------------------------------------------------------------------------------
# datos de incidencia y defunciones totales
#------------------------------------------------------------------------------
datosTotales <- datos %>% group_by(fecha) %>% 
  summarise(CasosAcum = sum(CasosAcum),
            CasosDia  = sum(CasosDia),
            DefAcum   = sum(DefAcum),
            DefDia    = sum(DefDia),
            Casos_7d  = sum(Casos_7d),
            Casos_14d = sum(Casos_14d),
            Def_7d    = sum(Def_7d),
            Def_14d   = sum(Def_14d)
  )

# grafico de casos
p <- ggplot(datosTotales, aes(as.Date(fecha), CasosDia)) + 
  geom_bar(stat = "identity",
           color = palette$green) +
  labs(title = "nº casos diarios en CV",
       subtitle = paste("fecha de generación: ", Sys.Date()),
       caption = "Fuente de Datos: https://dadesobertes.gva.es") +
  xlab("Fecha") + ylab("Casos Diarios") +
  scale_x_date(date_breaks = "1 month") +
  custom_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                       legend.position = "none",
                       panel.grid.major.x = element_blank())
png(filename = "./plots/casosDiarios.png", width = 1800, height = 900)
p
dev.off()
  


