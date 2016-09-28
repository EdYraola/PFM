
----------------------------------------#Importación y limpieza de datos---------------------------------------------

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

#Vemos que faltan los datos de los tres últimos meses de 2015
levels(Ft$MES)

---------------------------#Datos que faltaban de Octubre, Nobiembre y Diciembre------------------------------

# Importamos los datos de los meses que faltan de: http://www.ine.es/prodyser/micro_frontur.htm

setwd("~/Escritorio/Master/Proyecto/Frontur/Datos que faltaban.2015")
Oct <- read.table('Octubre_15.txt', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")
Nov <- read.table('Noviembre_15.txt', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")
Dic <- read.table('Diciembre_15.txt', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")

Oct$MES<-10
Nov$MES<-11
Dic$MES<-12

Ft2<-rbind(Oct,Nov,Dic)

Ft2$A0_1 <- NULL
Ft2$A0 <- NULL


# Modificamos los nombres de las columnas
colnames(Ft2)[colnames(Ft2)=="A0_7"] <- "TIPO.VIAJERO"
colnames(Ft2)[colnames(Ft2)=="A1"] <- "Entrada"
colnames(Ft2)[colnames(Ft2)=="Residencia"] <- "Pais"
colnames(Ft2)[colnames(Ft2)=="CCAA"] <- "CC_AA"
colnames(Ft2)[colnames(Ft2)=="A14"] <- "ALOJAMIENTOS"
colnames(Ft2)[colnames(Ft2)=="A15"] <- "MOTIVOS"
colnames(Ft2)[colnames(Ft2)=="A16"] <- "PAQUETE"
colnames(Ft2)[colnames(Ft2)=="A13_1"] <- "PERNOCTACIONES"
colnames(Ft2)[colnames(Ft2)=="Factor"] <- "VIAJEROS"

# Arreglamos los datos como en Frontur2015
Ft2$Entrada <- gsub("1",  "Aeropuerto", Ft2$Entrada)
Ft2$Entrada <- gsub("2",  "Carretera", Ft2$Entrada)
Ft2$Entrada <- gsub("3",  "Puerto", Ft2$Entrada)
Ft2$Entrada <- gsub("4",  "Tren", Ft2$Entrada)

Ft2$Pais <- gsub("^1$","126", Ft2$Pais)
Ft2$Pais <- gsub("^2$","103", Ft2$Pais)
Ft2$Pais <- gsub("^3$","108", Ft2$Pais)
Ft2$Pais <- gsub("^4$","110", Ft2$Pais)
Ft2$Pais <- gsub("^5$","113", Ft2$Pais)
Ft2$Pais <- gsub("^6$","115", Ft2$Pais)
Ft2$Pais <- gsub("^7$","121", Ft2$Pais)
Ft2$Pais <- gsub("^8$","123", Ft2$Pais)
Ft2$Pais <- gsub("^9$","125", Ft2$Pais)
Ft2$Pais <- gsub("^10$","199", Ft2$Pais)
Ft2$Pais <- gsub("^11$","154", Ft2$Pais)
Ft2$Pais <- gsub("^12$","132", Ft2$Pais)
Ft2$Pais <- gsub("^13$","199", Ft2$Pais)
Ft2$Pais <- gsub("^14$","302", Ft2$Pais)
Ft2$Pais <- gsub("^15$","899", Ft2$Pais)
Ft2$Pais <- gsub("^16$","899", Ft2$Pais)

Ft2$ALOJAMIENTOS <- gsub("6","11", Ft2$ALOJAMIENTOS )
Ft2$ALOJAMIENTOS <- gsub("7","6", Ft2$ALOJAMIENTOS )
Ft2$ALOJAMIENTOS <- gsub("8","9", Ft2$ALOJAMIENTOS )
Ft2$ALOJAMIENTOS <- gsub("9","11", Ft2$ALOJAMIENTOS )
Ft2$ALOJAMIENTOS <- gsub("0","99", Ft2$ALOJAMIENTOS )

Ft2$MOTIVOS <- gsub("0","99", Ft2$MOTIVOS )
Ft2$MOTIVOS <- gsub("1","31", Ft2$MOTIVOS )
Ft2$MOTIVOS <- gsub("4","33", Ft2$MOTIVOS )
Ft2$MOTIVOS <- gsub("5","34", Ft2$MOTIVOS )

Ft2$PAQUETE <- gsub("0","9", Ft2$PAQUETE)

Ft2$MES<-as.factor(Ft2$MES)
Ft2$VIAJEROS<-as.numeric(Ft2$VIAJEROS)
Ft2$ALOJAMIENTOS<-as.numeric(Ft2$ALOJAMIENTOS)
Ft2$PERNOCTACIONES<-as.factor(Ft2$PERNOCTACIONES)

Ft2$Provincia <- "00"
Ft2$Acceso <- NA
Ft2$Transporte <- NA
Ft2$DONANTE <- NA

# Reordenamos y creamos la tabla final
length(Ft2)
Ft2 <- Ft2[, c(10,4,11,3,12,5:8,13,1,14,2,9)]

Ft15<-rbind(Ft,Ft2)

#Comprobamos que Ft15 tiene todos los meses del año
levels(Ft15$MES)

--------------------------------------------------------------------------------------------------------------

#Guardamos la nueva tabla como csv.
write.csv(Ft15,file="Ft15.csv",row.names = FALSE)
