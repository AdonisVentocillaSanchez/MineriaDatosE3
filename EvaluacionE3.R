# CASO 1: Sistema Educativo del Perú

## Instalacion de paquetes
install.packages("DMwR")
install.packages("corrplot")
install.packages("PerformanceAnalytics")
install.packages("psych")
install.packages("factoextra")
install.packages("cluster")

library(DMwR)
library(corrplot)
library(PerformanceAnalytics)
library(psych)
library(factoextra)
library(cluster)

educacion=read.csv("https://raw.githubusercontent.com/VictorGuevaraP/Mineria-de-datos-2020/master/Sistema_Educativo_Peru.csv",sep=";")
head(educacion)
summary(educacion)
dim(educacion)

## Eliminamos las 2 primeras variables ya que no son numéricas
educacion_prep=educacion[,3:16]
head(educacion_prep)
dim(educacion_prep)

## Pruebas preliminares
cor(educacion_prep)

corrplot(cor(educacion_prep))

chart.Correlation(educacion_prep)

##  Para la decisión respecto a prueba de hipótesis
## P- Valor < nivel de significancia (0.05) Rechazo Ho
cortest(educacion_prep)
### 0 < 0.05 (es correcto), entonces se rechaza Ho. 
### Esto quiere decir que la correlación entre variables es igual a 0 
### por ende no se crearían los componentes principales y esta data lista para la fase del modelado

## Prueba de esfericidad de Barttlet
bartlett.test(educacion_prep)
### 2.2e-16 < 0.05 (es correcto), entonces se rechaza Ho
### Esto quiere decir que la matriz de correlaciones es distinta de la matriz de identidad

## Prueba KMO (Kaiser Meyer Olkin)
KMO(educacion_prep)
### Overall MSA = 0.74 > 0.5 (Se cumple),
### Se justifica el análisis de componentes principales



# CASO 2: Distrito

distritos<-read.csv("https://gist.githubusercontent.com/BenjiSantos/33fee8a00211146958990b66f864c70b/raw/a83679c2f26e016d7e71268badd388a5356d3db6/distritos.csv", sep = ";")

summary(distritos)
head(distritos)

distritosFilter=distritos[,2:8]

head(distritosFilter)
chart.Correlation(distritosFilter)
cor(distritosFilter)

distritosStandar<-scale(distritosFilter)

#Estandarisamos los datos para aplicar K-means,
#para ellos es necesario quitar la variable cualitativa
#
head(distritosStandar)

### Aplicanción del método del codo para ver la cantidad óptima de clusters
fviz_nbclust(distritosStandar, kmeans, method = "wss")
### Aplicación del método silhouette para ver la cantidad óptima de clusters
fviz_nbclust(distritosStandar, FUNcluster=kmeans, method="silhouette")+theme_classic()

grupo=kmeans(distritosStandar,2)
grupo

grupoFilter=grupo$cluster
grupoFilter

clusplot(distritosStandar, grupoFilter)


rownames(distritosStandar) <- distritos[,1]
fviz_cluster(grupo, data = distritosStandar)

distriKMEANS= cbind(distritosStandar, grupo$cluster)
head(distriKMEANS)

fviz_cluster(grupo, data = distriKMEANS)

ggplot(data = distritosFilter, aes(x = sinelect, y = sinagua, color = grupo$cluster)) +
  geom_point(size = 2) + theme_light()+geom_text(aes(label = distritos[,1]), size = 4) 

ggplot(data = distritosFilter, aes(x = ocu_vivi, y = pobpjov, color = grupo$cluster)) +
  geom_point(size = 2) + theme_light()+geom_text(aes(label = distritos[,1]), size = 4) 

# K-MEDOIDES CLUSTERING (PAM)

distritosPAM = pam(distritosStandar, 2)

distritosPAM$medoids
fviz_cluster(distritosPAM)

distriPAM= cbind(distritosStandar, distritosPAM$clustering)
head(distriPAM)

# CLARA

distritosCLARA = clara(distritosStandar, 2)
fviz_cluster(distritosCLARA)

distriCLARA= cbind(distritosStandar, distritosCLARA$clustering)
head(distriCLARA)

fviz_cluster(distritosStandar, data = distriCLARA)
  



