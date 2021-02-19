# generación de gráficos de incidencia y defunciones en CV

library(dplyr)
library(ggplot2)
library(gridExtra)
library(plotly)

source("customTheme.R")

caption <- "Autor: @vi_castellar \n Fuente: Elaboración propia, Datos: https://dadesobertes.gva.es"

# importar datos fuente gva
datos <- read.csv("./data/datosEdad.csv", stringsAsFactors = FALSE)



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


# incidencia global por edad
p <- ggplot(datosEdad, aes(as.Date(fecha), CasosDia, color = GrupoEdad)) + geom_bar(stat = "identity") +
  facet_wrap(. ~ GrupoEdad) + 
  labs(title = "nº casos PDIA+ por grupos de edad en CV",
       subtitle = paste("fecha de generación: ", Sys.Date()),
       caption = caption) +
  xlab("Fecha") + ylab("Casos Diarios") +
  scale_x_date(date_breaks = "1 month") +
  custom_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                       legend.position = "none",
                       panel.grid.major.x = element_blank())
png(filename = "./plots/casosXGruposEdad.png", width = 1800, height = 900)
p
dev.off()

# incidencia últimos 15 días
p <- datosEdad %>% filter(as.Date(fecha) >= as.Date(max(datosEdad$fecha)) - 14) %>% 
  ggplot(., aes(as.Date(fecha), CasosDia, color = GrupoEdad, fill = GrupoEdad)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=CasosDia), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_wrap(. ~ GrupoEdad) + 
  labs(title = "nº casos PDIA+ diarios por grupos de edad en CV últimos 15 días",
       subtitle = paste("fecha de generación: ", Sys.Date()),
       caption = caption) +
  xlab("Fecha") + ylab("Casos Diarios") +
  scale_x_date(date_breaks = "1 day") +
  custom_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                       legend.position = "none",
                       panel.grid.major.x = element_blank())
png(filename = "./plots/casosXGruposEdadUltimos15d.png", width = 1800, height = 900)
p
dev.off()

# defunciones global por edad
p <- ggplot(datosEdad, aes(as.Date(fecha), DefDia, color = GrupoEdad)) + geom_bar(stat = "identity") +
  facet_wrap(. ~ GrupoEdad) + 
  labs(title = "nº defunciones diarias por grupos de edad en CV",
       subtitle = paste("fecha de generación: ", Sys.Date()),
       caption = caption) +
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

# defunciones últimos 15 días
p <- datosEdad %>% filter(as.Date(fecha) >= as.Date(max(datosEdad$fecha)) - 14) %>% 
  ggplot(., aes(as.Date(fecha), DefDia, color = GrupoEdad, fill = GrupoEdad)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=DefDia), position=position_dodge(width=0.9), vjust=-0.25) +
  facet_wrap(. ~ GrupoEdad) + 
  labs(title = "nº de defunciones por grupos de edad en CV últimos 15 días",
       subtitle = paste("fecha de generación: ", Sys.Date()),
       caption = caption) +
  xlab("Fecha") + ylab("Casos Diarios") +
  scale_x_date(date_breaks = "1 day") +
  custom_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                       legend.position = "none",
                       panel.grid.major.x = element_blank())
png(filename = "./plots/defuncionesXGruposEdadUltimos15d.png", width = 1800, height = 900)
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
p1 <- ggplot(datosTotales, aes(as.Date(fecha), CasosDia)) + 
  geom_bar(stat = "identity",
           color = palette$green) +
  labs(title = "nº casos PDIA+ diarios en CV",
       subtitle = paste("fecha de generación: ", Sys.Date()),
       caption = caption) +
  xlab("Fecha") + ylab("Casos Diarios") +
  scale_x_date(date_breaks = "1 month") +
  custom_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                       legend.position = "none",
                       panel.grid.major.x = element_blank())

# grafico de defunciones
p2 <- ggplot(datosTotales, aes(as.Date(fecha), DefDia)) + 
  geom_bar(stat = "identity",
           color = palette$red) +
  labs(title = "nº de defunciones diarias en CV",
       subtitle = paste("fecha de generación: ", Sys.Date()),
       caption = caption) +
  xlab("Fecha") + ylab("Defunciones Diarias") +
  scale_x_date(date_breaks = "1 month") +
  custom_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                       legend.position = "none",
                       panel.grid.major.x = element_blank())





# gráfico de incidencia 14 días
p3 <- ggplot(datosTotales, aes(as.Date(fecha), Casos_14d / 5003769 * 1e5)) + 
  geom_bar(stat = "identity",
           color = palette$lightorange) +
  labs(title = "tasa PDIA+ 14 días por cada 100.000 habitantes en CV",
       subtitle = paste("fecha de generación: ", Sys.Date()),
       caption = caption) +
  xlab("Fecha") + ylab("nº casos x 100.000 habitantes") +
  scale_x_date(date_breaks = "1 month") +
  custom_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                       legend.position = "none",
                       panel.grid.major.x = element_blank())


# gráfico razón de tasas
p4 <- ggplot(datosTotales, aes(as.Date(fecha), c(rep(NA, 7), diff(log(Casos_14d), lag = 7) + 1))) + 
  geom_line(stat = "identity",
           color = palette$gold) +
  labs(title = "razón de tasas PDIA+ 14 días por cada 100.000 habitantes en CV",
       subtitle = paste("fecha de generación: ", Sys.Date()),
       caption = caption) +
  xlab("Fecha") + ylab("razón de tasas") +
  scale_x_date(date_breaks = "1 month") +
  geom_smooth(span = 0.25, color = palette$skyblue) +
  custom_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                       legend.position = "none",
                       panel.grid.major.x = element_blank())


# gráfico índice de fatalidad
p5 <- ggplot(datosTotales, aes(as.Date(fecha), DefAcum / CasosAcum * 1e2)) + 
  geom_smooth(span = 10,
              color = palette$red,
              level = 0.95) +
  labs(title = "evolución del índice de fatalidad en CV",
       subtitle = paste("fecha de generación: ", Sys.Date()),
       caption = caption) +
  xlab("Fecha") + ylab("índice de fatalidad") +
  scale_x_date(date_breaks = "1 month") +
  custom_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                       legend.position = "none",
                       panel.grid.major.x = element_blank())

# gráfico casos acumulados
p6 <- ggplot(datosTotales, aes(as.Date(fecha), CasosAcum)) + 
  geom_line(stat = "identity",
              color = palette$green,
              size = 1.25) +
  labs(title = "evolución del número de casos PDIA+ acumulados en CV",
       subtitle = paste("fecha de generación: ", Sys.Date()),
       caption = caption) +
  xlab("Fecha") + ylab("número de casos") +
  scale_x_date(date_breaks = "1 month") +
  custom_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                       legend.position = "none",
                       panel.grid.major.x = element_blank())


png(filename = "./plots/resumenSituacion.png", width = 1800, height = 900)
grid.arrange(p1, p2, p3, p6, p5, p4, ncol = 3, nrow = 2)
dev.off()


# grñaficos de incidencias 14D por grupos de edad
datosEdad <- merge(x = datosEdad, y = pobXGEdad, by = "GrupoEdad")
datosEdad <- datosEdad %>% 
  mutate(Inc_14d = Casos_14d / PobbEdad * 1e5)
p3 <- ggplot(datosEdad, aes(as.Date(fecha),Inc_14d, color = GrupoEdad)) +
  geom_bar(stat = "identity") +
    facet_wrap(. ~ GrupoEdad) + 
    labs(title = "Tasa PDIA+ 14d x 100.000 habitantes por grupos de edad en CV",
         subtitle = paste("fecha de generación: ", Sys.Date()),
         caption = caption) +
    xlab("Fecha") + ylab("Incidencia 14d") +
    scale_x_date(date_breaks = "1 month") +
    custom_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                         legend.position = "none",
                         panel.grid.major.x = element_blank())
png(filename = "./plots/Incidencia14DXGruposEdad.png", width = 1800, height = 900)
p3
dev.off()


# gráfico índice de fatalidad por grupos de edad
p5 <- ggplot(datosEdad, aes(as.Date(fecha), DefAcum / CasosAcum * 1e2, color = GrupoEdad)) + 
  #facet_wrap(.~GrupoEdad) +
  geom_line(size = 1.2) +
  labs(title = "evolución del índice de fatalidad en CV",
       subtitle = paste("fecha de generación: ", Sys.Date()),
       caption = caption) +
  xlab("Fecha") + ylab("índice de fatalidad") +
  scale_x_date(date_breaks = "1 month") +
  custom_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                       # legend.position = "none",
                       panel.grid.major.x = element_blank())
png(filename = "./plots/TasaFatalidadDXGruposEdad.png", width = 1800, height = 900)
p5
dev.off()
