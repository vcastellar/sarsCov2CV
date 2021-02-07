# sarsCov2CV
Datos y gráficos sobre la evolución de Covid19 en la Cominidad Valenciana
La fuente de los datos es https://dadesobertes.gva.es. Esta fuente proporciona datos acumulados de casos positivos y fallecimientos diarios. 

## Disclaimer
Hay que tener en cuenta que, para algunas fechas, la fuente no proporciona información; para estas fechas, se imputan los valores acumulados por interpolación. El resto de campos se calculan a partir de los datos acumulados

## Datos
En el directorio **datos** tenemos los siguientes ficheros:
+ **datos.csv** con los siguientes campos:
  + **GrupoEdad**: grupo de edad de las observaciones
  + **Sexo**: Variable sexo: *Hombre*, o *Mujer*
  + **PorcCasos**: Porcentaje de casos acumulados en la fecha y grupo de Edad-Sexo correspondiente sobre el total
  + **CasosAcum**: Casos positivos acumulados en la fecha y grupo de Edad-Sexo correspondiente
  + **PorcDefun**: Porcentaje de defunciones acumuladas en la fecha y grupo de Edad-Sexo correspondiente sobre el total
  + **DefAcum**: Casos de fallecidos acumulados en la fecha y grupo de Edad-Sexo correspondiente
  + **fecha**: fecha de la información en formato "yyyy-mm-dd"
  + **CasosDia**: Casos nuevos notificados en la fecha y grupo de Edad-Sexo correspondiente. Se calcula como la diferencia entre el valor de la observación vigente y el valor de la observación conocida inmediatamente anterior.
  + **DefDia**: Nuevas defunciones notificadas en la fecha y grupo de Edad-Sexo correspondiente. Se calcula como la diferencia entre el valor de la observación vigente y el valor de la observación conocida inmediatamente anterior.
  + **Casos14d**: Suma de los últimos 14 días de *CasosDia*
  + **Def14d**: Suma de los últimos 14 días de *DefDia*
  + **Casos7d**: Suma de los últimos 7 días de *CasosDia*
  + **Def7d**: Suma de los últimos 7 días de *DefDia*
  + **PoblacionCV**: número de habitantes en la Comunidad Valenciana a fecha 2020-01-01. Fuente: https://es.wikipedia.org/wiki/Anexo:Comunidades_y_ciudades_aut%C3%B3nomas_de_Espa%C3%B1a
