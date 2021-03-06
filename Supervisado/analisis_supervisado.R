#title: "Analisis_911"
#author: "Cesar Velasco - Belen Paredes"
#date: "1/11/2021"

#INTRODUCCIÓN

#En este análisis supervisado  se utiliza un  dataset de las llamadas 
#al 911 del condado de Montgomery en la Commonwealth of Pennsylvania. 
#Los datos se recopilan desde el 12 de diciembre de 2015 hasta el 20 
#de Julio de 2020. 

#Instalacion paquetes y librerias

#Paquetes vistos en clases

install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

#Paquetes  investigados

#********* paquetes para trabajar con Fechas *********
#https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html
#https://www.doctormetrics.com/lubridate-manipulando-fechas-en-r/
#https://www.rdocumentation.org/packages/lubridate/versions/1.7.9.2
#*****************************************************
install.packages("lubridate")
library(lubridate)    

#Libreria tidyr
library(tidyr)#https://rpubs.com/jaortega/151936  separar datos

#Libreria scales
library(scales)# https://rpubs.com/luis_bolanos/537899  para manejar porcentajes

#lectura del dataset 911.csv
emergency_calls <- read.csv("911.csv")

# Informacion del dataset
dim(emergency_calls)
str(emergency_calls)
head(emergency_calls)


#Realizamos la lectura de los datos a analizar, verificamos la relevancia 
#de las columnas y su significado.

#Explicación de los atributos:

#-lat:       Latitud del lugar de la llamada
#-Ing:       Longitud del lugar de la llamada
#-desc:      Descripción de la llamada de emergencia 
#-zip:       Código postal
#-Title:     Tipo de llamada de emergencia(EMS: Servicio médico de emergencia, Fire: Accidente de incendio, Trafic: Accidente de   tráfico)
#-TimeStamp: AAAA-MM-DD HH: MM: SS
#-Twp:       Municipio
#-addr:      Dirección 
#-e:         Variable ficticia (siempre 1)



#Creción de nuevas variables a partir de variables existentes y eliminacion  de  variables innecesarias.

#Separacion la columna titlle en dos (Types,Subtypes)
emergency_calls <- separate(emergency_calls,title,c("Types","Subtypes"),sep=":")

#Separacion la columna timeStamp en dos (Date_Calls, Hour_Calls)
emergency_calls <- separate(emergency_calls,timeStamp,c("Date_Calls","Hour_Calls"),sep=" ")

# Elimanacion columna "e"
emergency_calls$e <- NULL

#Nueva Dimension del DataSet
dim(emergency_calls)

#Cambio de clase a "date" para trabajo con Fechas (lubridate)
emergency_calls$Date_Calls <- as.Date(emergency_calls$Date_Calls)

emergency_calls$Hour_Calls <- hms(emergency_calls$Hour_Calls)


num_filas <- dim(emergency_calls)


#LLAMADAS MENSUALES

#Grafico del numero de llamadas por mes (duracte los 5 años)
total_call_per_month <-emergency_calls%>%
  group_by(month(Date_Calls))%>%
  count()%>%
  arrange(desc(n))

# ******* pagina para el ggplot_bar - ggplot2 *******
#https://www.r-graph-gallery.com/218-basic-barplots-with-ggplot2.html
#https://rstudio-pubs-static.s3.amazonaws.com/345827_ef9eb854d51943ca9433ff814e1fdf2a.html
#http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
#****************************************************

ggplot(total_call_per_month, aes(x=total_call_per_month$`month(Date_Calls)`, y=n)) +
  geom_bar(stat ="identity", fill="tomato3") +
  scale_x_continuous(breaks=seq(0, 12, 1)) +
  labs(title="Number of Emergency Calls per Month", 
       subtitle="Diciembre 2015 - Julio 2020") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

total_call_per_month

#Llamadas  Mensuales  

#-La frecuencia de las llamadas al 911  en  Enero es mayor mientras 
#que  en noviembre la frecuencia  baja

#numero de llamadas por años

total_calls_per_year <- emergency_calls%>%
  group_by(year(Date_Calls))%>%
  count()

ggplot(total_calls_per_year, aes(x=`year(Date_Calls)`, y=n)) +
  geom_bar(stat ="identity", fill="tomato3") +
  labs(title="Number of Emergency Calls per Year", 
       subtitle="2015 - 2020") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

total_calls_per_year

#Llamadas  Años

#-En el año 2015 y año 2020 el registro de llamadas corresonde a 1 mes 
#y 7 meses respectivamente, se puede observar en el grafico que el mayor 
#numero de llamadas se dio en el año 2018 y el menor numero de llamadas 
#en el año 2017


#Grafico numero de llamadas por mes para el 2016
calls_per_month_2016 <- emergency_calls%>%
  select(Date_Calls)%>%
  filter(Date_Calls>= "2016-01-01" & Date_Calls<="2016-12-31")%>%
  group_by(month=floor_date(Date_Calls, "month"))%>%
  count()

ggplot(calls_per_month_2016, aes(x=month, y=n)) +
  geom_bar(stat ="identity", fill="tomato3") +
  labs(title="Number of Emergency Calls", 
       subtitle="Year 2016") + 
  scale_y_continuous(breaks=seq(0, 15000, 3000))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Llamadas  Año 2016

#-En enero del 2016 se registra el mayor numero de llamadas 13096 
#de un total de 142360

#Grafico numero de llamadas por mes para el 2017
calls_per_month_2017 <- emergency_calls%>%
  select(Date_Calls)%>%
  filter(Date_Calls>= "2017-01-01" & Date_Calls<="2017-12-31")%>%
  group_by(month=floor_date(Date_Calls, "month"))%>%
  count()

ggplot(calls_per_month_2017, aes(x=month, y=n)) +
  geom_bar(stat ="identity", fill="tomato3") +
  labs(title="Number of Emergency Calls", 
       subtitle="Year 2017") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  scale_y_continuous(breaks=seq(0, 15000, 3000))

#Llamadas  Año 2017

#-En diciembre del 2017 se registra el mayor numero de llamadas 
#12941 de un total de 140343

#Grafico numero de llamadas por mes para el 2018
calls_per_month_2018 <- emergency_calls%>%
  select(Date_Calls)%>%
  filter(Date_Calls>= "2018-01-01" & Date_Calls<="2018-12-31")%>%
  group_by(month=floor_date(Date_Calls, "month"))%>%
  count()

ggplot(calls_per_month_2018, aes(x=month, y=n)) +
  geom_bar(stat ="identity", fill="tomato3") +
  labs(title="Number of Emergency Calls", 
       subtitle="Year 2018") + 
  scale_y_continuous(breaks=seq(0, 15000, 3000))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Llamadas  Año 2018

#-En marzo del 2018 se registra el mayor numero de llamadas 14923 
#de un total de 151527

#Grafico numero de llamadas por mes para el 2019
calls_per_month_2019 <- emergency_calls%>%
  select(Date_Calls)%>%
  filter(Date_Calls>= "2019-01-01" & Date_Calls<="2019-12-31")%>%
  group_by(month=floor_date(Date_Calls, "month"))%>%
  count()

ggplot(calls_per_month_2019, aes(x=month, y=n)) +
  geom_bar(stat ="identity", fill="tomato3") +
  labs(title="Number of Emergency Calls", 
       subtitle="Year 2019") + 
  scale_y_continuous(breaks=seq(0, 15000, 3000))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Llamadas  Año 2019

#-En octubre del 2019 se registra el mayor numero de llamadas 13425 
#de un total de 149118

#Grafico numero de llamadas por mes para el 2020
calls_per_month_2020 <- emergency_calls%>%
  select(Date_Calls)%>%
  filter(Date_Calls>= "2020-01-01" & Date_Calls<="2020-12-31")%>%
  group_by(month=floor_date(Date_Calls, "month"))%>%
  count()

ggplot(calls_per_month_2020, aes(x=month, y=n)) +
  geom_bar(stat ="identity", fill="tomato3", width=18) +
  labs(title="Number of Emergency Calls", 
       subtitle="Year 2020") + 
  scale_y_continuous(breaks=seq(0, 15000, 3000))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Llamadas  Año 2020

#-En enero del 2020 se registra el mayor numero de llamadas 12208 de 
#un total de 72258

#contamos el numero de llamadas por Township
aux <- emergency_calls %>%
  group_by(twp) %>%
  count()%>%
  arrange(desc(n))
head(aux)

#aux contiene el numero total de twp (69)
aux
# We found the town with the most emergency calls to 911
mas_llamadas<-aux %>%
  head(1,1)

mas_llamadas
print(paste("The town with the most 911 calls in Montgomery County, Pennsylvania es : ", mas_llamadas$twp,"con", mas_llamadas$n, "llamadas", "(Total Calls 911:",num_filas[1],")"))

# Graficamos las 5 ciudades con mas llamadas al 911

ggplot(aux [1:5,], aes(x=twp, y=n)) +
  geom_bar(stat ="identity", fill="tomato3", width=0.5) +
  scale_y_continuous(breaks=seq(0, 70000, 10000)) +
  labs(title="Towns with the most calls to 911") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

dim(aux)

#Grafico de las direcciones que han realizado mas llamadas

direcciones_masllamadas <- emergency_calls %>%
  group_by(addr) %>%
  count()%>%
  arrange(desc(n))
head(direcciones_masllamadas)

#direcciones_llamadas contiene el numero total de add (41292)
direcciones_masllamadas
fil <- dim(direcciones_masllamadas)#


# Encontramos la dirección  con mas llamadas de emergencia al 911
top1 <- direcciones_masllamadas %>%
  head(1,1)

#Direccion con mas llamadas al 911  con respecto al total de direcciones (41292)
print(paste("The address with the most calls to 911: ", top1$addr,"con",top1$n, "llamadas","(Total address :",fil[1],")"))

# Grafica de las direcciones VS cantidad de llamadas
ggplot(direcciones_masllamadas [1:5,], aes(y=addr, x=n)) +
  geom_bar(stat ="identity", fill="tomato3", width=0.5) +
  theme(axis.text.x = element_text(angle=65, vjust=0.53, size =7 ))+
  scale_x_continuous(breaks=seq(0, 7000, 1000))+
  labs(title="Towns with the most calls to 911") 

# Encontramos el numero de llamadas segun el tipo de emergencia
freqt.calls <-as.data.frame(table(emergency_calls$Types))

colnames(freqt.calls) <- c('Type_emergencia', 'Number_of_calls')

#Tipo de accidente conmas llamadas
print(paste("The type of accident with the most calls to 911: ", freqt.calls$`Type_emergencia`[1],"con",freqt.calls$`Number_of_calls`[1], "llamadas"))

# Encontramos en porcentaje el numero de llamadas segun su tipo de emergencia
porcentaje_typescall <- freqt.calls %>%
  mutate (Porcentaje = (freqt.calls$`Number_of_calls`/(num_filas[1])*100) )
head(porcentaje_typescall)

# Representacion grafica del porcentaje de llamadas segun el tipo de emergencia

ggplot(porcentaje_typescall,aes(x="",y=Porcentaje, fill=Type_emergencia,))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(Porcentaje/100,0.01)),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("salmon","steelblue","gray"))+
  theme_void()+
  labs(title="PORCENT 3 TYPES OF 991 CALLS", subtitle = "TOTAL 991 CALLS" )



#METODO SUPERVISADO

# REGRESION LINEAL

#En el eje x se representa el mes y e el eje y el numero de llamadas, se pretende encontrar una funcion que nos permita conocer o estimr el numero de llamasdas

total_calls_per_month <- emergency_calls%>%
  select(Date_Calls)%>%
  filter(Date_Calls>= "2016-01-01" & Date_Calls<="2020-12-31")%>%
  group_by(month=floor_date(Date_Calls, "month"))%>%
  count()


ggplot(total_calls_per_month, aes(x=month, y=n)) +
  geom_point(colour = "red", size = 3) +
  geom_path()

num_of_month <- matrix(1:55, nrow=55, ncol=1)
num_of_month <- as.data.frame(num_of_month)



aux <- cbind(num_of_month, total_calls_per_month)
aux$month<- NULL
aux

plot(x=aux$V1, y=aux$n)

#regresison lineal
mylm <- lm(n ~ ., data = aux)

mylm$coefficients

new <- aux[16,]
predict(mylm, new)

abline(mylm$coefficients, col="red")


summary(mylm)

#En base a la informacion obtenida por el metodo, se concluye que  no 
#se puede predecir elnumero de llamadas al 911 utilizando el metodo de 
#regresion lineal, como se puede observar el valor de R^2 es lejano 
#a 1 (mientras mas cercano a 1 mejor) y el valor de p-value es mucho 
#mayor a 0.05 



#[1] Dataset :  https://www.kaggle.com/mchirico/montcoalert