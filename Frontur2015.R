# Importamos librerias 

library(dplyr)
library(ggplot2)
library(reshape2)
library(rgdal)
library(RColorBrewer)
library(ggmap)
library(scales)
library(party)
library(randomForest)
library(class)
library(caret)

# Importamos los datos de http://estadisticas.tourspain.es/es-es/estadisticas/frontur/microdatos/paginas/default.aspx
setwd("~/Escritorio/Master/Proyecto/Frontur")
Ft <- read.table('Frontur2015.txt', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")
# colClasses = "character" para que NO se omitan los ceros iniciales en DESTINO.PRINCIPAL

head(Ft)

# Modificamos los nombres de las columnas
colnames(Ft)[colnames(Ft)=="DESTINO.PRINCIPAL"] <- "Destino"
colnames(Ft)[colnames(Ft)=="VIAS.ACCESO"] <- "Acceso"
colnames(Ft)[colnames(Ft)=="PAIS.RESIDENCIAS"] <- "Pais"
colnames(Ft)[colnames(Ft)=="TIPO.TRANSPORTE"] <- "Transporte"
colnames(Ft)[colnames(Ft)=="VIA.ENTRADA"] <- "Entrada"

# Arreglamos un poco los datos 
Ft$Entrada <- gsub("1",  "Aeropuerto", Ft$Entrada)
Ft$Entrada <- gsub("2",  "Carretera", Ft$Entrada)
Ft$Entrada <- gsub("3",  "Puerto", Ft$Entrada)
Ft$Entrada <- gsub("4",  "Tren", Ft$Entrada)

Ft$MES<-as.factor(Ft$MES)
Ft$VIAJEROS<-as.numeric(Ft$VIAJEROS)
Ft$ALOJAMIENTOS<-as.numeric(Ft$ALOJAMIENTOS)
Ft$PERNOCTACIONES<-as.factor(Ft$PERNOCTACIONES)

# Eliminamos las columnas que no interesan
Ft$Id <- NULL
Ft$AÑO <- NULL

# Subdividimos la variable Destino por CC_AA y Provincia. (Ver PDF del diseño de los datos. pg.5)
Ft$CC_AA<-substr(Ft$Destino, 1, 2)
Ft$Provincia<-substr(Ft$Destino, 3, 4)

head(Ft)
mean(Ft$VIAJEROS)

# Reordenamos la tabla
length(Ft)
Ft <- Ft[, c(1:2,14:15,3:13)]

# Eliminamos Destino
Ft$Destino <- NULL

------------------------------------------#Corrección a la muestra donada-------------------------------------

# Seleccionamos un mes concreto 
#(Para cambiar el mes se tiene que modificar el nº de mes en los sripts de las filas 62, 105(opcional) y 143)
tmp<-Ft[Ft$MES=="8",] 

# Aplicamos los filtros que queramos (Jugar con esto)
tmp<-tmp[!grepl("[1-4]",tmp$PERNOCTACIONES),] #Viajeros que pernoctan mas de una semana
tmp<-tmp[grepl("1",tmp$ALOJAMIENTOS),] #Viajeros alojados en hoteles
tmp<-tmp[grepl("31",tmp$MOTIVOS),] #Turistas 
# Muestra donada a las Provincias
table(tmp$Provincia=="00",tmp$CC_AA!="00") #Vemos que hay Provincias con valor 00 incluso cuando CC_AA es distinto de 00
# Cuando esto ocurre el valor del nº de viajeros corresponde a muestra donada.
table(tmp$Provincia=="00",tmp$CC_AA)  #Vemos cuantos viajeros por CC_AA hay con Provincia=="00" (incluido CC_AA=00)

# Por tanto, la cantidad total de muestra donada a toda una CC_AA tiene que repartirse proporcionalmente
# entre todas las provincias de esa CC_AA. 
# Para ello tenemos que multiplicar el nº de viajeros 
# de cada Provincia por el Factor = ((Muestra donada/Muestra recogida) + 1))
# Muestra donada = Suma de viajeros con Provincia=00
# Muestra recogida = Suma de viajeros con Provincia!=00

tmp$CC_AA<-as.numeric(tmp$CC_AA)
tmp$Provincia<-as.numeric(tmp$Provincia)

# Creamos una función que genera la lista de factores de cada CC_AA 
# que posteriormente tendremos que multiplicar por cada Provincia.

foo <- function(tmp){
  u=0
  for (i in 1:18){ 
    a<-sum(tmp[tmp$CC_AA==i,][tmp[tmp$CC_AA==i,]$Provincia==0,]$VIAJEROS) #Suma de viajeros con Provincia=00 (muestra donada)
    b<-sum(tmp[tmp$CC_AA==i,][tmp[tmp$CC_AA==i,]$Provincia!=0,]$VIAJEROS) #Suma de viajeros con Provincia!=00 (muestra recogida)
    u[i]=((a/b)+1); #Vector de factores
  }
  return(u) 
}

df <- data.frame(foo(tmp)) #Tabla de factores
df<-cbind(1:18,df) # Añadimos una columna que corresponde a los valores de las CC_AA.
colnames(df)[colnames(df)=="1:18"] <- "CC_AA"
colnames(df)[colnames(df)=="foo.tmp."] <- "Factor"

tmp<-merge(tmp,df, by = "CC_AA") #Unimos las dos tablas por la CC_AA
tmp$VIAJEROS<-tmp$VIAJEROS*tmp$Factor #Multiplicamos el nº de Viajeros por el factor de correción obtenido.
tmp$Factor<- NULL #Ya no necesitamos la columna Factor

#Creamos una tabla que nos de el nº total de viajeros por Provincia en Agosto (ya que antes nos quedamos solo con este mes).
tmp<-dcast(tmp, Provincia ~ MES , value.var = "VIAJEROS", fun.aggregate = sum)  
sum(tmp$`8`) #Dato de interés. Número total de viajeros en el mes de Agosto
tmp$Provincia<-as.numeric(tmp$Provincia) #Para que no de fallos en el merge con provincia de mas adelante

------------------------------------------#Visualicación de España------------------------------------------------------- 

#Mapa España Google
esp <- geocode('España', source = "google")
map.esp<- get_map(location = as.numeric(esp),
                  color = "color",
                  maptype = "roadmap",
                  scale = 2,
                  zoom = 6)

# Shapefiles de provincias españolas: http://www.arcgis.com/home/item.html?id=83d81d9336c745fd839465beab885ab7

#Leemos el Shapefile de provincias

provincias <- readOGR(dsn = "Provincias", "Provincias_ETRS89_30N")

class(provincias)
#slotNames(provincias)
provincias@data
#plot(provincias)

class(provincias$Cod_CCAA)
provincias$Cod_CCAA<-as.numeric(provincias$Cod_CCAA)

#GGPLOT España
points <- spTransform(provincias , CRS("+proj=longlat +datum=WGS84"))
points <- fortify(points) #Esto convierte el shapefile con todos sus atributos en un dataframe con el que ggmap puede operar.
points$id<-as.numeric(points$id)
points$id<-points$id+1 #Hay que sumarle 1 porque si no coge el 0, que no es ninguna provincia.

# Left join (con merge a veces se ordenan mal los datos y la visualización da fallos)
# points2 <- merge(points, tmp, by.x='id', by.y='Provincia', all.x=T)
points2 <- left_join(points, tmp, by = c("id" = "Provincia")) 

#Si quisieramos fijar un valor de corte del nº de viajeros
#points2 <- points2[points2$`8`<=2e+05,] 

# Spectral plot. Escalamos el total de Viajeros a la paleta de colores.
ggmap(map.esp) + geom_polygon(aes(x=long,y=lat, group=group, fill=`8`), data=points2, color='black') +
  scale_fill_distiller(breaks = pretty_breaks(n = 6), palette='Spectral') 


---------------------#Si queremos que aparezca el valor numérico dentro de cada provincia---------------------

centers<-points2[,c(1,2,6,8)] #tabla con las longitudes, latitudes, ids, y nº de viajeros de cada provincia

#función que genera los valores medios de la anterior tabla
#así obtenemos el centro de cada provincia
foo <- function(centers){
  u=0
  for (i in 1:52){ 
    a<-colMeans(centers[centers$id==i,]) 
    u[i]<-merge(a,a[i+1])
  }
  return(u) 
}

#Creamos la nueva tabla y la reordenamos. 
#Seguramente haya una manera mejor pero daba fallos incluso anidando las siguientes expresiones.
centers <- foo(centers)
centers<-data.frame(centers)
centers<-t(centers)
centers<-data.frame(centers)

colnames(centers)[colnames(centers)=="X1"] <- "long"
colnames(centers)[colnames(centers)=="X2"] <- "lat"
colnames(centers)[colnames(centers)=="X3"] <- "id"
colnames(centers)[colnames(centers)=="X4"] <- "8"
rownames(centers) <- c()
centers$`8`<-round(centers$`8`,1) #Redondeamos los valores del nº de viajeros
head(centers)

# Spectral plot + valor del nº de viajeros
ggmap(map.esp) + geom_polygon(aes(x=long,y=lat, group=group, fill=`8`), data=points2, color='black') +
  scale_fill_distiller(breaks = pretty_breaks(n = 6), palette='Spectral') +
  geom_text( data=centers, hjust=0.5, vjust=-0.5, aes(x=long, y=lat, label=`8`), colour="black", size=4)


--------------------------------#Visualización de Cataluña--------------------------------------------------- 

cata <- geocode('Cataluña', source = "google")
map.cata<- get_map(location = as.numeric(cata),
                   color = "color",
                   maptype = "roadmap",
                   scale = 2,
                   zoom = 8)

#Asignamos a points el shapefile de Cataluña
points<-provincias[provincias$Cod_CCAA == 9, ]
points <- spTransform(points , CRS("+proj=longlat +datum=WGS84"))
points <- fortify(points)
points$id<-as.numeric(points$id)
points$id<-points$id+1 #Hay que sumarle 1 porque si no coge el 0, que no es ninguna provincia.

#Total de viajeros en un Mes concreto 
tmp<-Ft[Ft$MES=="8",]
tmp<-dcast(tmp, Provincia ~ MES , value.var = "VIAJEROS", fun.aggregate = sum)
tmp$Provincia<-as.numeric(tmp$Provincia) #Para que no de fallos en el merge con provincia de mas adelante

# Left_join (con merge a veces se ordenan mal los datos y la visualización da fallos)
# points2 <- merge(points, tmp, by.x='id', by.y='Provincia', all.x=T)
points2 <- left_join(points, tmp, by = c("id" = "Provincia"))

# Spectral plot. #Escalamos el total de Viajeros a la paleta de colores 
ggmap(map.cata) + geom_polygon(aes(x=long,y=lat, group=group, fill=`8`), data=points2, color='black') +
  scale_fill_distiller(breaks = pretty_breaks(n = 4),palette='Spectral')  +
  geom_text( data=centers, hjust=0.5, vjust=-0.5, aes(x=long, y=lat, label=`8`), colour="black", size=4)

-----------------------------------------#Pruebas varias-------------------------------------------
  
#Si quisieramos quitar campos que no proceden (PERNOCTACIONES=999, PAQUETE=9)
Ft<-Ft[!grepl("999",Ft$PERNOCTACIONES),]
Ft<-Ft[!grepl("9",Ft$PAQUETE),]

#tmp<-dcast(Ft, MES + Destino + Acceso ~ PERNOCTACIONES, value.var = "VIAJEROS", fun.aggregate = sum)

#?rbind

#Filtrando por una sola CC_AA (DESTINO que empieza por ...
mad <- Ft[grepl("13",Ft$CC_AA),] #13 Madrid
mad$CC_AA<-NULL
table(mad$Provincia)
sum(mad$VIAJEROS)

cat <- Ft[grepl("09",Ft$CC_AA),] #09 Cataluña
cat$CC_AA<-NULL
table(cat$Provincia)
sum(cat$VIAJEROS)


#Filtrando por alojados solo en hoteles (Hoteles=1)
tmp2 <- Ft[grepl("^1$",Ft$ALOJAMIENTOS),]
tmp2<-dcast(tmp2, MES +  PAQUETE ~ PERNOCTACIONES, value.var = "VIAJEROS", fun.aggregate = sum)
ggplot(tmp2, aes(x = MES, y = `2`, col=PAQUETE)) + geom_point() + geom_smooth() 

ggplot(tmp2, aes(x = MES, y = `2`, fill = PAQUETE, col=PAQUETE)) +
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")

#ggplot(tmp, aes(VIAJEROS)) + geom_histogram(binwidth = 50) + facet_grid(MES~.)
#ggplot(tmp, aes(x = VIAJEROS, fill=MES)) + geom_density(alpha = 0.5)

#PERNOCTACIONES=1
#tmp <- tmp[grepl("1",tmp$PERNOCTACIONES),]
#tmp <- tmp[tmp$PERNOCTACIONES %in% c(5, 6),]

ggplot(tmp, aes(x = MES, y = `5`)) + geom_point() + geom_smooth() 
class(tmp$`1`)
class(Ft$VIAJEROS)

----------------------------------------------#Modelos--------------------------------------------------------

#tmp <- Ft[grepl("^1$",Ft$ALOJAMIENTOS),] #Turistas alojados en Hoteles
tmp <- Ft[grepl("^8$",Ft$MES),] #Turistas en Agosto
tmp$Destino <- NULL
tmp$MES <- NULL
tmp$PAQUETE<-as.factor(tmp$PAQUETE)
tmp$CC_AA<-as.factor(tmp$CC_AA)
tmp$PERNOCTACIONES<-as.factor(tmp$PERNOCTACIONES)
tmp$MOTIVOS<-as.factor(tmp$MOTIVOS)

#Vemos que nacionalidades tienen el mayor número de turistas
tmp2<-dcast(tmp, Pais ~ PERNOCTACIONES , value.var = "VIAJEROS", fun.aggregate = sum)
tmp2<- tmp2[!grepl("^0",tmp2$Pais),] #Quitamos turistas españoles

pais1<-head(tmp2[order(tmp2$`4`,decreasing=T),],20) #Nacionalidades con mayor número de turistas (de 4 a 7 noches)
pais2<-head(tmp2[order(tmp2$`5`,decreasing=T),],20) #Nacionalidades con mayor número de turistas (de 8 a 15 noches)
pais3<-head(tmp2[order(tmp2$`6`,decreasing=T),],20) #Nacionalidades con mayor número de turistas (de más de 15 noches)

pais1$Pais #Top 6 de Nacionalidades (de 4 a 7 noches)
pais2$Pais #Top 6 de Nacionalidades (de 8 a 15 noches)
pais3$Pais #Top 6 de Nacionalidades (de más de 15 noches)

#tmp3<-dcast(tmp, Pais + PAQUETE ~ CC_AA  , value.var = "VIAJEROS", fun.aggregate = sum)
#tmp3<-dcast(tmp, Pais + PAQUETE + PERNOCTACIONES ~ CC_AA  , value.var = "VIAJEROS", fun.aggregate = sum)

tmp3<-dcast(tmp, CC_AA  + PAQUETE + PERNOCTACIONES + MOTIVOS ~  Pais, value.var = "VIAJEROS", fun.aggregate = sum)
tmp3 <- tmp3[!grepl("^00$",tmp3$CC_AA),] #Nos quitamos los viajeros con destino distinto de España.

#Número total de turistas en Agosto 
sum(tmp3$`110`) #Francia
sum(tmp3$`125`) #Reino Unido
sum(tmp3$`126`) #Alemania 
sum(tmp3$`115`) #Italia
sum(tmp3$`121`) #Paises bajos
sum(tmp3$`154`) #Rusia


table(tmp3$PAQUETE, tmp3$PERNOCTACIONES)
prop.table(table(tmp3$PAQUETE, tmp3$PERNOCTACIONES))
max(prop.table(table(tmp3$PAQUETE, tmp3$PERNOCTACIONES)))

#Arbol
arbol <- ctree(`126`~  CC_AA + PAQUETE + PERNOCTACIONES + MOTIVOS, data = tmp3) #arbol de um pais determinado (126 = Alemania)
#Los Rusos=154 no pillan muchos paquetes
plot(arbol)

#Franceses
arbol <- ctree(`110`~ CC_AA + PAQUETE + PERNOCTACIONES + MOTIVOS, data = tmp3)
arbol <- ctree(`110`~ CC_AA + PAQUETE , data = tmp3)
arbol <- ctree(`110`~ CC_AA + PERNOCTACIONES , data = tmp3)
arbol <- ctree(`110`~ PAQUETE + PERNOCTACIONES , data = tmp3)
plot(arbol)  

max(tmp3$`110`)
head(tmp3[order(tmp3$`110`,decreasing=T),])


a<-filter(tmp3, CC_AA == "04", PERNOCTACIONES == 5)
a[,46]

plot(tmp3[37:48,3],tmp3[37:48,46]) #Viajeros alemanes en Baleares vs número de días
plot(tmp3[37:42,3],tmp3[37:42,46]) #Viajeros alemanes sin paquete en Baleares vs número de días
plot(tmp3[43:48,3],tmp3[43:48,46]) #Viajeros alemanes con paquete en Baleares vs número de días

plot(tmp3[37:48,3],tmp3[37:48,65]) #Viajeros alemanes en Rusos vs número de días

tmp3[grepl("04",tmp2$CC_AA),46]

tmp4 <- tmp[grepl("^110$",tmp$Pais),] #Franceses
tmp4<-dcast(tmp4, CC_AA  + PAQUETE + PERNOCTACIONES + MOTIVOS~  Pais, value.var = "VIAJEROS", fun.aggregate = sum)
tmp4 <- tmp4[!grepl("^00$",tmp3$CC_AA),] #Nos quitamos el 00 de las CCAA
class(tmp4$`110`)
tmp4$`110`<-as.numeric(tmp4$`110`)
hist(tmp3$`110`)

#RandomForest
rforest <- randomForest(`110`~ CC_AA + PAQUETE + PERNOCTACIONES + MOTIVOS, data = tmp3)
plot(rforest)

varImpPlot(rforest) 

filas.entrenamiento <- sample(1:nrow(tmp3), 0.8 * nrow(tmp3))

tmp3.train <- tmp3[filas.entrenamiento,]
tmp3.test  <- tmp3[-filas.entrenamiento,]

nrow(tmp3)
nrow(tmp3.train) 
nrow(tmp3.test)

reales <- tmp3.test$`126`
predichos <- predict(rforest, tmp3.test )

table(reales, predichos)
error.rate <- mean(reales != predichos) #error de validez del modelo
error.rate


train.control <- trainControl(method = "cv", number = 10)
model <- train(`110`~ CC_AA + PAQUETE + PERNOCTACIONES, data=tmp3, trControl=train.control, method="lm")
1# summarize results
print(model)

--------------------
--------------------
  
tmp4<-Ft[Ft$MES=="8",] #Seleccionamos un mes 

#Muestra donada a Provincias
table(tmp4$Provincia=="00",tmp4$CC_AA!="00") #Vemos si hay Provincias con valor 00 incluso cuando CC_AA es distinto de 00
table(tmp4$Provincia=="00",tmp4$CC_AA)  #Vemos cuantos viajeros por CC_AA hay con Provincia=="00" (incluido CC_AA=00)

tmp4$CC_AA<-as.numeric(tmp4$CC_AA)
tmp4$Provincia<-as.numeric(tmp4$Provincia)

sum(tmp4$VIAJEROS[tmp4$CC_AA==9]) #Total de viajeros en una CC_AA específica (Cataluña)

-----------------------------------------
#Para una sola CC_AA (Cataluña)
a<-tmp4[tmp4$CC_AA==9,] #Filtro por CC_AA
#Donaciones por Provincia
a<-a[a$Provincia==0,] #Filtro por Provincia==0
#Total de muestra donada a Cataluña
sum(a$VIAJEROS) #Total de viajeros en Cataluña con Provincia==0 
#Total de viajeros sin muestra en Cataluña
b<-tmp4[tmp4$CC_AA==9,]
b<-b[b$Provincia!=0,]
sum(b$VIAJEROS) 

#Corrección de muestra donada para una sola Provincia (Barcelona)
#Total de viajeros en Cataluña con Provincia==00 
a<-sum(tmp4[tmp4$CC_AA==9,][tmp4[tmp4$CC_AA==9,]$Provincia==0,]$VIAJEROS) 
#Total de viajeros en Cataluña en Provincia!=00
b<-sum(tmp4[tmp4$CC_AA==9,][tmp4[tmp4$CC_AA==9,]$Provincia!=0,]$VIAJEROS) 
#Total de viajeros en Cataluña en Provincia==08 (Barcelona)
c<-sum(tmp4[tmp4$CC_AA==9,][tmp4[tmp4$CC_AA==9,]$Provincia==8,]$VIAJEROS) 
#Total real de viajeros en Barcelona (corregido por la muestra donada)
((a/b) + 1)*c
-----------------------------------------
  
#Lista de factores de cada CC_AA que se tienen que multiplicar por cada Provincia
for (i in 1:18){ #Al empezar en 1 nos quitamos CC_AA=0
#Total de viajeros en Provincia==00 
a<-sum(tmp4[tmp4$CC_AA==i,][tmp4[tmp4$CC_AA==i,]$Provincia==0,]$VIAJEROS) #Suma de viajeros de Provincia=00
#Total de viajeros en Provincia!=00
b<-sum(tmp4[tmp4$CC_AA==i,][tmp4[tmp4$CC_AA==i,]$Provincia!=0,]$VIAJEROS) #Suma de viajeros de Provincia!=00
print((a/b)+1)
}


tmp4<-dcast(tmp4, CC_AA ~ Provincia , value.var = "VIAJEROS", fun.aggregate = sum)
tmp4[tmp4$CC_AA=="01",2]/tmp4[tmp4$CC_AA=="01",]

via$Provincia<-as.numeric(via$Provincia) #Para que no de fallos en el merge con provincia de mas adelante

# Merge
points2 <- merge(points, via, by.x='id', by.y='Provincia', all.x=T)



via2<-Ft[Ft$MES=="8",]
via2$CC_AA<-as.numeric(via2$CC_AA)
via2$Provincia<-as.numeric(via2$Provincia)
#via2<-dcast(via2, CC_AA ~ Provincia , value.var = "VIAJEROS", fun.aggregate = sum)
a<-via2$VIAJEROS[via2$CC_AA=="9"]

# i==CC_AA

for (i in 1:18){
  
    if(via2$Provincia=="0") a<-(sum(via2$VIAJEROS[via2$CC_AA==i])) 
      
    else b<-(sum(via2$VIAJEROS[via2$CC_AA==i])))
  
  print(a)
}

sum(via2$VIAJEROS[via2$CC_AA=="1"])

?ifelse
for (i in 1:18){
 # print(via2$VIAJEROS[via2$CC_AA==i])
  print(sum(via2$VIAJEROS[via2$CC_AA==i[via2$Provincia=="00"]]))
}

a<-sum(via2$VIAJEROS[via2$Provincia=="00"])

a<-sum(via2$VIAJEROS[via2$Provincia=="00"])
table(via2$Provincia)
b<-sum(via2$VIAJEROS[via2$Provincia!="00"])
c<-sum(via2$VIAJEROS[via2$Provincia=="08"])

via2$VIAJEROS<-via2$VIAJEROS*((a/b)+1)

z<-lm(`110`~ CC_AA, data = tmp3)
summary(z)

----------------------------
  
hist(sum(tmp$PAQUETE==1))
hist(replicate(100,tmp$PAQUETE==1))
tmp[tmp$PAQUETE==1,]
(tmp$PAQUETE==1)
hist(tmp$PAQUETE==1)
class(tmp$PAQUETE)
tmp$PAQUETE<-as.factor(tmp$PAQUETE)
tmp$PERNOCTACIONES<-as.factor(tmp$PERNOCTACIONES)

barplot(table(tmp$PAQUETE))
barplot(table(tmp$PERNOCTACIONES))

rnorm(10)

tmp[tmp$PERNOCTACIONES==999,] <- NULL

barplot(table(Ft$PAQUETE))
barplot(table(Ft$PERNOCTACIONES))