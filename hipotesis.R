base1,
x<-c(42.2,42.6,43.3,43.5,43.7,44.1,44.9,45.3,45.7,45.7,45.9,46,46.2,46.2,46.8,46.8,47.1,47.2)
y<-c(44,44,44,45,45,46,46,46,47,48,48,48,47,48,48,49,49,49)
mod1<- lm(y~x) #dependiente y, independiente x
summary(mod1)
anova(mod1)
show(mod1) #da bo y b1
names(mod1) #todo lo que arroja el modelo
mod1$coefficients

#################### ejercicio intrvalo b1

x1<-c(635,644,711,708,836,820,810,870,856,923,878,937,948)
y1<-c(100,93,88,84,77,75,74,63,57,55,47,43,38)
mod3<-lm(y1~x1)
summary(mod3)
mod3$coefficients

qt(.025,11) #valor de la tabla para alfa/2=.025,n-2
#intervalo de confianza
-.175645+(-2.200985*.01837)
-.175645-(-2.200985*.01837)

######ejercicio 2

x2<-c(5,12,14,17,23,30,40,47,55,67,72,81,96,112,127) #volumen de precipitacion
y2<-c(4,10,13,15,15,25,27,46,38,46,53,70,82,99,100) #volumen de escurrimiento
mod4<-lm(y2~x2)
summary(mod4)
mod4$coefficients
#intervalo de confianza
z<-qt(.025,13) #valor de la tabla para alfa/2=.025,n-2

####como el valor del estadistico es menor a .05 entonces se rechaza la hipotesis nula
#y aceptamos la hipotesis altrnativa, es decir tenemos una relacion util y podemos
#trabajar con este modelo

####intervalo de confianza
.8269731-(z*.03652)
.8269731+(z*.03652)

###ante un cambio en una unidad en el volumen de precipitacion
# el cambio en el volumen de escurrimiento  estara entre .7480764 y .9058698 

