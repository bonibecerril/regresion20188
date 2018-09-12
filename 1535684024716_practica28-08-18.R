require(ggplot2)
############################### problema 1 ##############################

## la relacion de eficiencia de un especimen de acero sumergido en un tanque de fosfato es el peso
#  del recubrimiento de fosfado dividido entre la perdida de metal (ambos en mg/pie^2). El articulo
#  aporta los siguientes datos sobre la temperatura del tanque (x) y la relacion de eficiencia (y)

x1<- c(170,172,173,174,174,175,176,177,180,180,180,180,180,181,181,182,182,182,182,184,184,185,186,188)
y1<- c(.84,1.31,1.42,1.03,1.07,1.08,1.04,1.80,1.45,1.60,1.61,2.13,2.15,.84,1.43,.90,1.81,1.94,2.68,1.49,2.52,3,1.87,3.08)

# a) construya graficas de tallo y hojas tanto de la relacion de temperatura como de la relacion de 
#    eficiencia, y comente las caracteristicas interesante.
boxplot(x1, y1)

# b) el valor de la relacion de eficienca ¿ estara determinado por completo y de forma unica por la 
#    tempera del tanque? Explique su razonamiento

mdt<- mean(x1)
mdr<- mean(y1)
p<- x1-mdt
s<- y1-mdr
pp<- p^2
ss<- s^2
smpp<- sum(p*s)
smss<- sum(pp*ss)
pearson<- smpp/smss
pearson


# c) construya una grafica de dispersion de los datos. ¿Sera que la relacion de eficiencia bien po-
#    dria pronosticarse mediante el valor de la temperatura? explique su razonamiento.

tabla<- data.frame(x1,y1)
ggplot(tabla, aes(x1,y1)) + 
  geom_point() +
  ggtitle("Gráfica de Dispersión") + 
  geom_smooth(method=lm)

# Con los datos que muestra la grafica, podemos notar que las variables se relacionan linealmente
# positivas con una fuerza media
#

############################# problema 2 ##################################

# un estudio reporta los datos en el cual se utilizo una mezcla de gasolina reformulada. considera 
# las siguientes observacines sobre edad(años) y emisiones de NO,(g/kWh)
motor<- c(1:10)
edad<- c(0,0,2,11,7,16,9,0,12,4)
lnbase<- c(1.72,4.38,4.06,1.26,5.31,.57,3.37,3.44,.74,1.24)
refor<- c(1.88,5.93,5.54,2.67,6.53,.74,4.94,4.89,.69,1.42)

#construya graficas de dispersion de emisiones de NO, contra edad. ¿cual parece ser la naturalez
#de la relacion entre estas dos variables?

tabla2<- data.frame(lnbase,edad)
ggplot(tabla2, aes(lnbase,edad)) + 
  geom_point() +
  ggtitle("Gráfica de Dispersión") + 
  geom_smooth(method=lm)

tabla3<- data.frame(refor,edad)
ggplot(tabla3, aes( refor,edad)) + 
  geom_point() +
  ggtitle("Gráfica de Dispersión") + 
  geom_smooth(method=lm)
# tomamos la linea base (lnbase) y la reformulada (refor)como NO y las comparamos con la edad
# lo que podemos observar en ambar graficas es una relacion negatica con fuerza debel.


################################## problema 3 ##############33##########

x3<-c(47,62,65,70,70,78,95,100,114,118,124,127,140,140,140,150,152,164,198,221)
y3<-c(38,62,53,67,84,79,93,106,117,116,127,114,134,139,142,170,149,154,200,215)

r3<-data.frame(x3,y3)

x11()
ggplot(r3,aes(x=x3,y=y3))+ geom_point()+geom_smooth(method = lm)

#Preguntas
#¿habra una fuerte relacion entre lo dos tipos de medicion?
#¿Sera que los dos metodos miden la misma cantidad?
#Respuesta
#Con base en el grafico de dispersion se aprecia que existe un relacion positiva alta 
#lo que tambien nos dice que con culaquiera de los dos metodos la probabilidad de obtener 
#resultados iguales es muy alta

######################### problema 4 ##############################

x4<-c(5.8,8.8,11,13.6,18.5,21,23.7,26,28.3,31.9,36.5,38.2,40.4) ###transpiracion
y4<-c(7.8,8.2,6.9,5.3,4.7,4.9,4.3,2.7,2.8,1.8,1.9,1.1,4) ###concentracion de amonio
xy4<-data.frame(x4,y4)
x11()
ggplot(xy4,aes(x=x4,y=y4))+ geom_point()+geom_smooth(method = lm)
#preguntas
#¿como describirias la relacion entre variables y sera que la regresion linal simple
#es una estrategia de modelado apropiada?

#De acuerdo a lo observado en el diagrama de dispersion tenemos un relacion negativa alta
#y si es adecuado el uso de la regresion lineal simplea 
#por que tenemos una relacion deterministica de dos variables una la cantidad de amonio en 
#rizosfera y otra la transpiracion de las poblaciones de plantas 

############################# problema 5 #################################

x5<-c(59,63,68,72,74,78,83)
y5<-c(118,182,247,208,197,135,132)
dat5<-data.frame(x5,y5)
dat5
####a)
x11()
plot(x5, y5, xlim =c(0,100), ylim=c(0,250))
####b)
x11()
plot(x5, y5, xlim =c(55,100), ylim =c(100,250))

##Por el hecho de quelos puntos se ven mejor distribuidos 
#se puede decir que es preferible la grafica del inciso b)
# aunque por el lado del inciso a) la grafica muestra de una 
#manera mas general la distribucion de los puntos en el plano completo.
####c) La relacion entre ambos vectores es muy dispersa y en ambas graficas se 
#puede apreciar que los puntos se ven iguales.

####################### problema 6 ################################

# donde x= frecuencia de resonancia de la raqueta
# y y=suma de la aceleracion pico a pico (vibracion el brazo)
#Analice las caracteristicas interesantes de los datos y la 
# grafica de dispersion

#SOLUCION
#Observando el grafico de dispersion que muestra el ejercicio podemos 
#que existe una relacion negativa alta lo que indica que las variables
#tienen una relacion inversa es decir si la resonancia de la raqueta aumenta, 
#las vibraciones del brazo disminuyen y viceversa , esto es interesante
#ya que considero que normalmente pensariamos que sucederia lo contrario 
 
################################# problema 7 ################################

x7<-c(2500,2501,2600,2400)
#recta de regresion
y7<-(1800+1.3*x7)
y7
xy7<-data.frame(x7,y7)
x11()
ggplot(xy7,aes(x=x7,y=y7)) + geom_point() + geom_smooth(method=("lm"))

#plot(x,y)

###a) cuando x=2500
#El valor esperado de la resistencia despues de 28 dias 
#cuando la resistencia acelerada es 2500 es de 5050.0

###b)cuando x=2501
#La resistencia despues de 28 dias al incrementarse
#una unidada en la resistencia acelerada es de 5051.3
#lo que quiere decir que crecio 1.3 aumentando una unidad

###c) cuando x=2600
#Para el incremento de 100 en la resistencia acelerada 
#es de 5180, lo cual se acerca mucho a lo que se espera
#aumentar de acuerdo a una unidad

###d) cuando x=2400
#Al disminuir en 100 la resistencia acelerada, la 
#resistencia es de 4920 es mucho menor a lo que se esperaba 
#que tenia que disminuir de acuerdo a lo que disminuye en
#una unidad

############################### problema 8 ####################


### a) P(y>5000 cuando x=2000)
x8<- c(2000)
y8<-(1800+1.3*x1)
pnorm(2000,5000,350)

### b) P(y>5000 cuando x=2500)
x8<- c(2500)
y8<-(1800+1.3*x1)
pnorm(2500,5000,350)
### c) 
x111<-c(2000, 2500)
#recta de regresion
y111<-(1800+1.3*x111)
y111

plot(x111,y111)
lines(x111,y111)
### d) P(y2>y1)=0.95

##############################################ejercicio 9
x9<-c(5:20)
y9=-.12+.095*x9
x11()
plot(x9,y9)
median(y9)


##si aumenta 1 pulg en la caida de presión
x91<-c(5:21)
y91=-.12+.095*x91
x11()
plot(x91,y91)
median(y91)
#al incrementar una pulg en la caída de presión, la velocidad de flujo aumenta tiene una relacion positiva

##si la caida de presion reduce en 5 pulg
x92<-c(5:15)
y92=-.12+.095*x92
x11()
plot(x92,y92)
median(y92)
#se espera una velocidad de.83(m^3/min) si la caida de presion reduce en 5 pulg


##cual es la velocidad de flujo esperada con la caida de presion de 10 pulg
x93<-c(5:10)
y93=-.12+.095*x93
x11()
plot(x93,y93)
median(y93)
#se espera una velocidad de.5925(m^3/min) con la caida de presion de 10 pulg


##cual es la velocidad de flujo esperada con la caida de presion de 15 pulg
x94<-c(5)
y94=-.12+.095*x94
x11()
plot(x94,y94)
median(y94)

#se espera una velocidad de .355(m^3/min) con la caida de presion de 15 pulg


############################### problema 10 ###############################
# y=4000+10x
# Y= observacion sobre el costo de una corrida 
# ¿podria ser el caso de que P(Y>5500 cuando x=100)=.05 
# y P(Y>6500 cuando x=200)=.10? Explique 
x10<-c(100, 200)
y10<-4000 + (10*x10)
#RESPUESTA
#El caso no se puede dar ya que para calular la probabilidad condicional
#se utiliza p(a interseccion b ) / p(b) y al sustituir
# el tamaño de la corrida en la ecuacion de costos, el costo es menor en 
#ambos casos por lo que la probabilidad de la interseccion es 0 y por lo
#tanto la probabiiidad para ambos casos es 0.
################################# problema 11 ############################
y11=5-(.1*(1))
y11
####4.9
y112=5-(.1*(10))
y112
###4
y113=5-(.1*(200))
y113

