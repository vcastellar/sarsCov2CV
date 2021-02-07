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
  + **fecha**: fecha de la información
