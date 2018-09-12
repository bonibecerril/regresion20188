x <- c (125.3,98.2,201.4,147.3,145.9,124.7,112.2,120.2,161.2,178.9,159.5,145.8,75.1,151.4,144.2,125,198.8,132.5,159.6,110.7)
y <- c(77.9,76.8,81.5,79.8,78.2,78.3,77.5,77,80.1,80.2,79.9,79,76.7,78.2,79.5,78.4,87.5,77,79,78.6)

k<- mean(x)
j<- mean(y)

tabla<- data.frame(x,y)

## Beta gorrito 1
install.packages("ggplot2")
library(ggplot2)
plot(x,y)
boxplot(x,y)
xx<- x-k
yy<- y-j
p1<- sum(xx*yy)
p2<- sum(xx^2)
B1<- p1/p2
B1


## Beta 0
g1<-sum(y)
g2<-B1*(sum(x))
B0<- (g1-g2)/20
B0

### calculen y gorrito
ygor<- (B0+B1*x)
ygor

### calculen los 
resi <- (y-ygor)
resi

## Grafica
x11()
ggplot(tabla, aes(x,y)) + 
  geom_point() +
  ggtitle("Gráfica de Dispersión") + 
  geom_smooth(method=lm)
### a la plataforma y al correo
