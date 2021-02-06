#Entregar la solución de la tarea en un archivo en formato de Word o PDF a través de Canvas. Corre el siguiente código en R para descargar los resultados del Censo 2020 de INEGI. 

#En esta tarea trabajaremos con estos datos. censo20.R 
rm(list=ls()) 

# Fija tu directorio de trabajo setwd("C:/clase/tareas/tarea1_2") 

# Descarga el archivo del censo 2020 por AGEB para 

#Nuevo Leon 
download.file(url= "https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/ageb_manzana/RESAGEBURB_19_2020_csv.zip", destfile="censo20_NL.zip", method="curl") 

#Descomprime el archivo .zip 
unzip(zipfile = "censo20_NL.zip", exdir = getwd())

#Lee el archivo que descomprimes.
d20 <-read.csv( paste("RESAGEBURB_19CSV20.csv", sep=""), encoding="UTF-8", na.strings="*", stringsAsFactors = FALSE) 
#Cargar librerias
library(tidyverse)
#colnames(d20)[colnames(d20)== "NOM_MUN"]<-"Municipios"
#(a) Filtra tu base de datos de tal forma que solamente despliegue los 
#resultados de las manzanas, elimina los totales por AGEB, localidad, 
#municipio y estado. Quédate solamente con las manzanas.
head(d20)

d20mza <-select(d20,NOM_MUN,MZA: VPH_SINTIC)
head(d20mza)

#(b) Filtra tu base de datos del punto anterior, de tal forma que contenga
#solamente las manzanas de los municipios de la ZMM (once o dieciocho municipios,
#según la definición que utilices) y guárdalo como un nuevo objeto. 
ZMM<-filter(d20mza,NOM_MUN %in% c("Apodaca", "Cadereyta", "Jiménez", "El Carmen", "García", "San Pedro Garza García",
    "General Escobedo", "Guadalupe", "Juárez", "Monterrey", "Salinas Victoria", 
    "San Nicolás de los Garza", "Santa Catarina", "Santiago")) 
head(ZMM)
#En este objeto selecciona solamente las columnas siguientes: grado promedio de
#escolaridad, población total, viviendas particulares habitadas, viviendas 
#con internet, promedio de ocupantes por cuarto, población sin afiliación a
#servicios de salud.
ZMM1<-select(ZMM,NOM_MUN,MZA,POBTOT,GRAPROES,VIVPAR_HAB,VPH_INTER,PSINDER,PRO_OCUP_C)
head(select(ZMM,NOM_MUN,MZA,POBTOT,GRAPROES,VIVPAR_HAB,VPH_INTER,PSINDER,PRO_OCUP_C))
colnames(ZMM1)[colnames(ZMM1)== "NOM_MUN"]<-"Municipios"
#(c) En la base de datos del inciso anterior, elimina las manzanas que contengan
#valores nulos en alguna de las variables que seleccionaste.

c<-na.omit(ZMM1)
c[c == "N/D"]<-0
###
#c$GRAPROES[c$GRAPROES %in% c("N/D")]<-0
c[c(4:8)]<-lapply(c[c(4:8)], as.integer)
#c$GRAPROES<-as.integer(c$GRAPROES)
anyNA(c)
#(d) Con esta base de datos final, genera una tabla que despliegue el mínimo, 
#máximo y promedio de grado promedio escolaridad por municipio a partir de las 
#manzanas. Es decir, agrega las manzanas por municipio y genera el mínimo, 
#máximo y promedio. Presenta tu tabla en los resultados, así como
#el código de R.

c %>% 
  group_by(Municipios )%>%
  summarise(manzanas=sum(MZA),Min_Escolaridad=min(GRAPROES),Maximo_Escolaridad=max(GRAPROES),
            Prom_Escolaridad=mean(GRAPROES))%>%
  arrange(desc(Maximo_Escolaridad))

ccount()
