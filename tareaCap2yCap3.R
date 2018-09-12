############################# tarea #####################
#Karen Nájera 
#Mariela Moreno
#Mayra Rocha
#Bonifacio Becerril 
install.packages("MASS")
library("MASS")
Animals
hills
install.packages("datasets.load")
library("datasets")
View(LakeHuron)
LakeHuron
x11()
par(mfrow=c(2,2), pch=16)
plot(LakeHuron,)
mtext("LAKEHURON",side=1,line = 6)
plot(mean(LakeHuron))
mtext("PROMEDIO DE LAKEHURON",side=1,line = 6)


x11()
plot(LakeHuron,type="p")
mtext("LAKEHURON",side=3,line = 1)
identify(LakeHuron)
############### respuesta maximo 2 minimo  90

################## problema 2 ######################
View(Animals)
x11()
par(mfrow=c(1,2))
plot(Animals$body,Animals$brain,pch=16,xlab = "peso del cuerpo", ylab = "peso del cerebro")
plot(log(Animals$body),log(Animals$brain),pch=16,xlab = "peso del cuerpo", ylab = "peso del cerebro", axes=F)
br<-10^seq(-1,4)
bo<-10^seq(-2,4)
axis(1,at=log(bo),lab=bo)
axis(2,at=log(br),lab=br)
box()
identify(log(Animals$body),log(Animals$brain), labels=row.names(Animals) )
####################problema 3 #################
#la base de datos possum ya no se encuentra disponible


####################problema 4 #################
xn<-rnorm(10)
xn1<-rnorm(sample(1:1000000,10), mean = 170,sd=4)
mean(x1)
sd(x1) 

####################problema 5 #################

x5<-rnorm(sample(1:1000,10))
x51<-rnorm(sample(1:1000,10))
x511<-rnorm(sample(1:1000,10))
x5111<-rnorm(sample(1:1000,10))

x52<-sample(1:1000,100)
x522<-sample(1:1000,100)
x5222<-sample(1:1000,100)
x52222<-sample(1:1000,100)

x53<-sample(1:1000,1000,replace = T)
x533<-sample(1:1000,1000,replace = T)
x5333<-sample(1:1000,1000,replace = T)
x53333<-sample(1:1000,1000,replace = T)
x11()
par(mfrow=c(3,4))
plot(x5)
plot(x51)
plot(x511)
plot(x5111)
plot(x52)
plot(x522)
plot(x5222)
plot(x52222)
plot(x53)
plot(x533)
plot(x5333)
plot(x53333)

##Se puede notar claramente que los primeros 4 graficos se aprecia mucho
#mejor ya que al utilizar la funcion rnorm 
#los datos ya tienes un comportamiento predeterminado, ademas de que
# se utilizaron menos datos


######################### problema 6 #######################
xuniforme<-runif(10)
xuniforme
 
xnormal<-rnorm(10)
xnormal
x11()
par(mfrow=c(1,2))
plot(xnormal)
mtext("distribucion normal",side=3,line = 1)
plot(xuniforme)
mtext("distribucion uniforme",side=3,line = 1)

x6<-runif(sample(1:1000,10))
x61<-runif(sample(1:1000,10))
x611<-runif(sample(1:1000,10))
x6111<-runif(sample(1:1000,10))

x62<-sample(1:1000,100)
x622<-sample(1:1000,100)
x6222<-sample(1:1000,100)
x62222<-sample(1:1000,100)
x63<-sample(1:1000,1000,replace = T)
x633<-sample(1:1000,1000,replace = T)
x6333<-sample(1:1000,1000,replace = T)
x63333<-sample(1:1000,1000,replace = T)
x11()
par(mfrow=c(3,4))
plot(x6)
plot(x61)
plot(x611)
plot(x6111)
plot(x62)
plot(x622)
plot(x6222)
plot(x62222)
plot(x63)
plot(x633)
plot(x6333)
plot(x63333)

#aunque cambia el comportamiento de los primeros 4 graficos,
#la cantidad de datos no permite notar un cambio significante
#continuan teniendo una mejor qpreciacion que el resto

################################## problema 8 ######################

View(hills)
x11()
par(mfrow=c(2,2))
hist(hills$dist,breaks = 10)
hist(hills$climb,breaks = 10)
hist(log(hills$dist),breaks = 10)
hist(log(hills$climb),breaks = 10)

x11()
par(mfrow=c(2,2))
qqnorm(hills$dist)
qqnorm(hills$climb)
qqnorm(log(hills$dist))
qqnorm(log(hills$climb))



x11()
par(mfrow=c(2,2))
plot(hills$dist)
plot(hills$climb)
plot(log(hills$dist))
plot(log(hills$climb))


#con base en el histograma , que al parecer es donde mejor se 
# observa el comportamiento de los datos, obtenemos que estos siguen
# una distribucion chi-cuadrada



##### ggplot2


install.packages("ggplot2")
install.packages("diamonds")
library(ggplot2)

library(diamonds)


ggplot(data=diamonds, aes(carat)) +xlim(0,3)



##

huron<- data.frame(year=1875:1972, level=LakeHuron)

x11()

ggplot(huron, aes(year))+
  
  geom_line(aes(y=level-5), colour="pink")+
  
  geom_line(aes(y=level+5), colour="green")


#####

require()


ggplot(data= anscombe, aes(x=x1, y=y1)) + geom_point(stat = "bin")


ggplot(data=anscombe, aes(x=x1, y=y1)) +
  
  geom_point(method="lm") + geom_smooth(method = "lm")


###


ggplot(anscombe, aes(x=x1, y=y1)) + geom_point() +
  
  scale_x_continuous("my label", limits = c(1, 15))


ggplot(anscombe, aes(x=x1, y=y1)) + geom_point() +
  
  scale_x_continuous(limits = c(1, 15))+
  
  scale_y_continuous(limits = c(1, 20))



ggplot(anscombe, aes(x=x1, y=y1)) + geom_point() +
  
  scale_x_log10() + scale_y_log10()

###TITULOS

anscombe

ggplot(anscombe, aes(x=x2, y=4)) + geom_point() +
  
  opts(title = "Datos", aspect.ratio= 1)

######
set.seed(1410)
dsmall <- diamonds[sample(nrow(diamonds), 100), ]
x11()
qplot(carat, price, data = diamonds)
qplot(log(carat), log(price), data = diamonds)
qplot(carat, x * y * z, data = diamonds)


x11()
qplot(carat, price, data = dsmall, colour = color)#define a cada punto un color
qplot(carat, price, data = dsmall, shape = cut)#define a cada fonto una forma o figura 

#También puede establecer manualmente la estética usando I (), por ejemplo, color = I ("rojo")
#o tamaño = I (2).
#hacer un color semitransparente puede usar la estética alfa, que toma
#un valor entre 0 (completamente transparente) y 1 (opaco completo). Sus
#a menudo es útil para especificar la transparencia como una fracción, por ejemplo, 1/10 o 1/20, como
#el denominador
x11()
qplot(carat, price, data = diamonds, colour = I('red'),alpha=I(1/10))#define el color de los puntos, alpha define donde se consentran los valores
qplot(carat, price, data = diamonds, alpha = I(1/10))
qplot(carat, price, data = diamonds, alpha = I(1/200))

#Aquí presentaremos los geoms más comunes y útiles, organizados por
#dimensionalidad de los datos con los que trabajan. Los siguientes geoms te permiten
#para investigar las relaciones bidimensionales.
#geom = "point"
#geom = "boxplot"
#geom = "path" and geom = "line"
#c()combina múltiples geoms 
qplot(carat, price, data = dsmall, geom = c("point", "smooth"))
qplot(carat, price, data = diamonds, geom = c("point", "smooth"))


#method = "loess"el valor predeterminado para n pequeño, usa una regresión local uniforme.
#Se pueden encontrar más detalles sobre el algoritmo utilizado wiggliness de la línea es controlada por el parámetro span, que varía
#desde 0 (excesivamente ondulado) a 1 (no tan ondulado),
x11()
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      span = 0.2)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"),
      span = 1)
qplot(color, price / carat, data = diamonds, geom = "jitter",
      alpha = I(1 / 5))
qplot(color, price / carat, data = diamonds, geom = "jitter",
      alpha = I(1 / 50))
qplot(color, price / carat, data = diamonds, geom = "jitter",
      alpha = I(1 / 200))

# El primer diagrama es un simple diagrama de barras de
#color del diamante, y el segundo es un gráfico de barras de color diamante ponderado por quilate.
x11()
qplot(color, data = diamonds, geom = "bar")
qplot(color, data = diamonds, geom = "bar", weight = carat) + #peso= quilate
  scale_y_continuous("carat")
# Conjunto de datos de economicos 
# Que contiene datos economicos de los EE. UU. Medidos durante los ultimos 40 años. La figura muestra dos tramas
# de desempleo en el tiempo, ambas producidas usando geom = "linea". El primero muestra una tasa de desempleo y el 
# segundo muestra la mediana del numero de semanas desempleadas. Ya podemos ver algunas diferencias en estas dos variables,
# particularmente en el ultimo pico, donde el porcentaje de desempleo es mas bajo que en los picos precedentes, pero la 
# duracion del desempleo es alta.
qplot(date, unemploy / pop, data = economics, geom = "line")
qplot(date, uempmed, data = economics, geom = "line")

# Para examinar esta relacion en mayor detalle, dibujaremos ambas series de tiempo en la misma ventana. 
# La solucion es unir puntos adyacentes en el tiempo con segmentos de linea, formando un diagrama de ruta.

year <- function(x) as.POSIXlt(x)$year + 1900
qplot(unemploy / pop, uempmed, data = economics,
      geom = c("point", "path"))
qplot(unemploy / pop, uempmed, data = economics,
      geom = "path", colour = year(date)) + scale_area()

## Ejercicio 2.6 Faceting
# Faceting adopta un enfoque alternativo: crea tablas de graficos dividiendo los datos en subconjuntos
# y mostrando el mismo grafico para cada subconjunto en una disposicion que facilita la comparacion.
qplot(carat, data = diamonds, facets = color ~ .,
      geom = "histogram", binwidth = 0.1, xlim = c(0, 3))
# Lo anteior ilustra esta tecnica con dos graficos, conjuntos de histogramas que muestran la distribucion 
# de quilates condicional en el color.

qplot(carat, ..density.., data = diamonds, facets = color ~ .,
      geom = "histogram", binwidth = 0.1, xlim = c(0, 3))
# La figura anterior muestra proporciones, por lo que es mas facil comparar distribuciones independientemente de 
# la abundancia relativa de diamantes de cada color. La sintaxis de ..density .. es nueva. El eje y del histograma 
# no proviene de los datos originales, sino de la transformacion estadistica que cuenta el numero de observaciones 
# en cada contenedor. Usando ..density ... le dice a ggplot2 que mapee la densidad al eje y en lugar del uso 
# predeterminado de conteo.


## Ejercicios 2.7 other option
qplot(
  + carat, price, data = dsmall,
  + xlab = "Price ($)", ylab = "Weight (carats)",
  + main = "Price-weight relationship")
qplot(
  + carat, price/carat, data = dsmall,
  + ylab = expression(frac(price,carat)),
  + xlab = "Weight (carats)",
  + main="Small diamonds",
  + xlim = c(.2,1))
  qplot(carat, price, data = dsmall, log = "xy")
  
  