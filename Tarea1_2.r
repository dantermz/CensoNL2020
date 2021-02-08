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
ZMM1<-select(ZMM,NOM_MUN,MZA,POBTOT,TOTHOG,GRAPROES,VIVPAR_HAB,VPH_INTER,PSINDER,PRO_OCUP_C)
head(select(ZMM,NOM_MUN,MZA,POBTOT,TOTHOG,GRAPROES,VIVPAR_HAB,VPH_INTER,PSINDER,PRO_OCUP_C))
colnames(ZMM1)[colnames(ZMM1)== "NOM_MUN"]<-"Municipios"
#(c) En la base de datos del inciso anterior, elimina las manzanas que contengan
#valores nulos en alguna de las variables que seleccionaste.

c<-na.omit(ZMM1)
c[c == "N/D"]<-0
###
#c$GRAPROES[c$GRAPROES %in% c("N/D")]<-0
c[c(4:9)]<-lapply(c[c(4:9)], as.integer)
#c$GRAPROES<-as.integer(c$GRAPROES)
anyNA(c)
#(d) Con esta base de datos final, genera una tabla que despliegue el mínimo, 
#máximo y promedio de grado promedio escolaridad por municipio a partir de las 
#manzanas. Es decir, agrega las manzanas por municipio y genera el mínimo, 
#máximo y promedio. Presenta tu tabla en los resultados, así como
#el código de R.

c %>% 
  group_by(Municipios)%>%
  summarise(Num_Manz=sum(MZA),Min_Esc=min(GRAPROES),Max_Esc=max(GRAPROES),
  Prom_Esc=mean(GRAPROES))%>%
  arrange(desc(Max_Esc))
 

#Regresión Lineal 
#(a) Con la base de datos del ejercicio anterior, cálcula las siguientes dos 
#nueva variables: 
#a) promedio de viviendas particulares habitadas con internet.
c %>% 
  group_by(Municipios)%>%
  summarise(Num_Manz=sum(MZA),Min_Esc=min(GRAPROES),Max_Esc=max(GRAPROES),
            Prom_Esc=mean(GRAPROES),Prom_Viv_Int=mean(VPH_INTER))%>%
            arrange(desc(Prom_Viv_Int))

#b)porcentaje de la población sin afiliación a servicios de salud.
c %>% 
  group_by(Municipios)%>%
  summarise(Num_Manz=sum(MZA),Min_Esc=min(GRAPROES),Max_Esc=max(GRAPROES),
            Prom_Esc=mean(GRAPROES),Prom_Viv_Int=mean(VPH_INTER), Sin_Serv_Salud=mean(PSINDER))%>%
  arrange(desc(Sin_Serv_Salud))

#(b) Estima una regresión lineal de grado promedio de escolaridad por manzana 
#sobre porcentaje de vivendas con internet. Presenta el resultado e interpreta 
#el coeficiente.
cor(c$VPH_INTER/100,c$GRAPROES)

#(c) Dibuja un plot donde grafiques grado promedio de escolaridad contra 
#porcentaje de viviendas, agregando al plot de puntos una línea en color 
#rojo que represente tu línea de regresión. ¿Qué te dice esteplot?
plot(c$VPH_INTER/100,c$GRAPROES)
abline(lm(c$GRAPROES~c$VPH_INTER), col="red")
ols1<- lm(c$GRAPROES~c$VPH_INTER, data=c)
summary(ols1)

#(d) Estima una regresión lineal de grado promedio de escolaridad por manzana 
#sobre porcentaje de viviendas con internet, 
cor(c$VPH_INTER/100,c$GRAPROES)

#promedio de ocupantes por cuarto
cor(c$PRO_OCUP_C,c$GRAPROES)
plot(c$PRO_OCUP_C,c$GRAPROES)
abline(lm(c$GRAPROES~c$PRO_OCUP_C), col="red")
ols2<- lm(GRAPROES ~ PRO_OCUP_C, data=c)
summary(ols2)

#porcentaje de población sin afiliación servicios de salud.
cor(c$PSINDER,c$GRAPROES)
plot(c$PSINDER,c$GRAPROES)
abline(lm(c$GRAPROES~c$PSINDER), col="red")
ols3<- lm(GRAPROES ~ PSINDER, data=c)
summary(ols3)


#Presenta los resultados e interpreta. ¿Cómo cambiaron los estimadores respecto 
#al ejercicio anterior? ¿Por qué?
png("tarea2.png", width=480, height=480)

par(mar = c(4,4,2,2), mfrow=c(2,2))

plot(c$VPH_INTER/100,c$GRAPROES)
abline(lm(c$GRAPROES~c$VPH_INTER), col="red")

plot(c$PRO_OCUP_C,c$GRAPROES)
abline(lm(c$GRAPROES~c$PRO_OCUP_C), col="red")

plot(c$PSINDER,c$GRAPROES)
abline(lm(c$GRAPROES~c$PSINDER), col="red")

dev.off()


#(e) ¿Tu regresión tiene el sesgo de la variable omitida o no? ¿Por qué? 
#¿Son confiables tus estimadores? Explique.