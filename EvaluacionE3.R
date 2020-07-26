# CASO 1: Sistema Educativo del Perú

## Instalacion de paquetes
install.packages("DMwR")
install.packages("corrplot")
install.packages("PerformanceAnalytics")
install.packages("psych")

library(DMwR)
library(corrplot)
library(PerformanceAnalytics)
library(psych)

educacion=read.csv("https://raw.githubusercontent.com/VictorGuevaraP/Mineria-de-datos-2020/master/Sistema_Educativo_Peru.csv",sep=";")
head(educacion)
summary(educacion)
dim(educacion)

## Eliminamos las 2 primeras variables ya que no son numéricas
educacion_prep=educacion[,3:16]
head(educacion_prep)
dim(educacion_prep)
cor(educacion_prep)

corrplot(cor(educacion_prep))
chart.Correlation(educacion_prep)

##  Para la decisión respecto a prueba de hipótesis
## P- Valor < nivel de significancia (0.05) Rechazo Ho
cortest(educacion_prep)
### 0 < 0.05 (es correcto), entonces se rechaza Ho. 
### Esto quiere decir que la correlación entre variables es igual a 0 
### por ende no se crearían los componentes principales y esta data lista para la fase del modelado


