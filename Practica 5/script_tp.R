#Ejercicio de TCL en R: 

#a) caso n=1

#Con run if generamos los 1000 valores aleatorios 
x1<-runif(1000)

# Histograma de frecuencias relativas
hist(x1, prob = TRUE)

# Histogramas de frecuentas
hist(x1)

xbarra <- x1


#b) caso n=2

# Dos variables uniformes (0,1)
aa<-runif(2)
# El promedio de estas
x2<-mean(aa)

# Ahora repetimos 1000 veces. 

matriz2 <- matrix(0, nrow=2, ncol=1000)
#Genero dos columnas, una para cada observacion
for(i in 1:1000){ matriz2[,i] <- runif(2)}
matriz2[,1:4]
# aplicamos el promedio por columnas. Obtendremos un vector de 1000 coordenadas
# cada una es una variable aleatoria que es el promedio de dos uniformes (0,1) 
#independientes
# colMeans promedia todas las columnas
xbarra2<-colMeans(matriz2)

# Histograma
hist(xbarra2, prob=TRUE)
# Curve(dnorm(x, promedio, desvio standar))
curve(dnorm(x , mean(xbarra2) , sd(xbarra2)),add=TRUE)

curve(dnorm(x,0.5, sqrt(1/(12*2))),add = TRUE, col = 2)

#c) caso n=5
matriz5<-matrix(0,nrow=5, ncol=1000)
for(i in 1:1000){ matriz5[,i]<-runif(5)}
#chequeamos
dim(matriz5)

xbarra5<-colMeans(matriz5)
hist(xbarra5,prob=TRUE)
curve(dnorm(x,mean(xbarra5),sd(xbarra5)),add=TRUE)
curve(dnorm(x, 0.5, sqrt(1/(12*5)) ), add=TRUE)

#comparamos los 3 histogramas
par(mfrow=c(1,3))
hist(x1,probability=T)
hist(xbarra2,probability=T)
hist(xbarra5,probability=T)

par(mfrow=c(1,1))

#d) caso n=30
matriz30<-matrix(0,nrow=30, ncol=1000)
for(i in 1:1000){ matriz30[,i]<-runif(30)}

xbarra30<-colMeans(matriz30)
hist(xbarra30, prob=TRUE)

#En curve dnorm(x, ) pues usa funciones matematicas, la variable
curve(dnorm(x,mean(xbarra30),sd(xbarra30)),add=TRUE)


# caso n=500
matriz500<-matrix(0,nrow=500, ncol=1000)
for(i in 1:1000){ matriz500[,i]<-runif(500)}

xbarra500<-colMeans(matriz500)
hist(xbarra500,prob=TRUE)
curve(dnorm(x, mean(xbarra500), sd(xbarra500)),
      add=TRUE)


# caso n=1200
matriz1200<-matrix(0,nrow=1200, ncol=1000)
for(i in 1:1000){ matriz1200[,i]<-runif(1200)}

xbarra1200<-colMeans(matriz1200)
hist(xbarra1200,prob=TRUE)
curve(dnorm(x, mean(xbarra1200), sd(xbarra1200)),
      add=TRUE)



par(mfrow=c(2,3))
hist(x1,probability=T)
hist(xbarra2,probability=T)
hist(xbarra5,probability=T)
hist(xbarra30,probability=T)
hist(xbarra500,probability=T)
hist(xbarra1200,probability=T)

#Mejor aun, pongamos la misma escala en x para comparar

par(mfrow=c(2,3))
hist(x1,probability=T,xlim=c(0,1))
hist(xbarra2,probability=T,xlim=c(0,1))
hist(xbarra5,probability=T,xlim=c(0,1))
hist(xbarra30,probability=T,xlim=c(0,1))
hist(xbarra500,probability=T,xlim=c(0,1))
hist(xbarra1200,probability=T,xlim=c(0,1))

#pongamos tabi?n la misma escala en y

MM<-max(hist(xbarra1200,plot=F)$density)

par(mfrow=c(2,3))
hist(xbarra,probability=T,xlim=c(0,1),ylim=c(0,MM))
hist(xbarra2,probability=T,xlim=c(0,1),ylim=c(0,MM))
hist(xbarra5,probability=T,xlim=c(0,1) ,ylim=c(0,MM))
hist(xbarra30,probability=T,xlim=c(0,1) ,ylim=c(0,MM))
hist(xbarra500,probability=T,xlim=c(0,1) ,ylim=c(0,MM))
hist(xbarra1200,probability=T,xlim=c(0,1) ,ylim=c(0,MM))

# o bien, hagamos los graficos de estimaciones 
# de la densidad los 6 grupos de datos en el mismo grafico
# density hace un "histograma suavizado" que es mas facil de mirar

par(mfrow=c(1,1))
plot(density(xbarra1200),xlim=c(0,1),lwd=2,main="Densidades estimadas para los 
promedios de n observaciones U(0,1)")
lines(density(xbarra500),col=2,lwd=2)
lines(density(xbarra30),col=6,lwd=2)
lines(density(xbarra5),col=4,lwd=2)
lines(density(xbarra2),col=5,lwd=2)
lines(density(xbarra),col=3,lwd=2)
legend( "topright", c("n = 1", "n = 2","n = 5", "n = 30","n = 500","n = 1200"), col = c(3,5,4,6,2,1),lty = rep(1,6), lwd =rep(2,6),cex=0.8)

# para verlos en la misma escala podemos hacer los boxplots
boxplot(xbarra,xbarra2,xbarra5,
        xbarra30,xbarra500,xbarra1200,
        names=c("n=1", "n=2","n=5", "n=30",
                "n=500","n=1200"))

#medias muestrales
mean(xbarra)
mean(xbarra2)
mean(xbarra5)
mean(xbarra30)
mean(xbarra500)
mean(xbarra1200)

#varianzas muestrales

var(xbarra)
var(xbarra2)
var(xbarra5)
var(xbarra30)
var(xbarra500)
var(xbarra1200)
1/12
1/(12*2)
1/(12*5)
1/(12*30)
1/(12*500)
1/(12*1200)

par(mfrow=c(2,3))
qqnorm(xbarra)
qqline(xbarra,col="red", lwd=3)
qqnorm(xbarra2)
qqline(xbarra2,col="red",lwd=3)
qqnorm(xbarra5)
qqline(xbarra5,col="red",lwd=3)
qqnorm(xbarra30)
qqline(xbarra30,col="red",lwd=3)
qqnorm(xbarra500)
qqline(xbarra500,col="red",lwd=3)
qqnorm(xbarra1200)
qqline(xbarra1200,col="red",lwd=3)


qqplot(runif(1000),xbarra)
cuantiles_uniformes <-
        qunif(seq(0,1,length = 1000))
qqplot(cuantiles_uniformes,xbarra)


#g) Estandaricemos a una escala adecuada para poder apreciar el comportamiento aleatorio de xbarra

e1<-(xbarra-0.5)*sqrt(1)/sqrt(1/12)
e2<-(xbarra2-0.5)*sqrt(2)/sqrt(1/12)
e5<-(xbarra5-0.5)*sqrt(5)/sqrt(1/12)
e30<-(xbarra30-0.5)*sqrt(30)/sqrt(1/12)
e500<-(xbarra500-0.5)*sqrt(500)/sqrt(1/12)
e1200<-(xbarra1200-0.5)*sqrt(1200)/sqrt(1/12)

par(mfrow=c(2,3))
hist(e1,prob=TRUE)
hist(e2,prob=TRUE)
hist(e5,prob=TRUE)
hist(e30,prob=TRUE)
hist(e500,prob=TRUE)
hist(e1200,prob=TRUE)


par(mfrow=c(1,1))

plot(density(e1200),lwd=2,main="Densidades estimadas para los promedios estandarizados segun TCL
de n observaciones U(0,1)",ylim=c(0,0.41))
lines(density(e500),col=2,lwd=2)
lines(density(e30),col=6,lwd=2)
lines(density(e5),col=4,lwd=2)
lines(density(e2),col=5,lwd=2)
lines(density(e1),col=3,lwd=2)
lines(seq(-4,4,by=0.01),dnorm(seq(-4,4,by=0.01)),col=7,lwd=2)
legend( "topright", c("n = 1", "n = 2","n = 5", "n = 30","n = 500","n = 1200","densidad N(0,1)"), col = c(3,5,4,6,2,1,7),lty = rep(1,7), lwd =rep(2,7),cex=0.8)


par(mfrow=c(1,1))
boxplot(e1,e2,e5,e30,e500,e1200, names=c("n=1", "n=2","n=5", "n=30","n=500","n=1200"))

##################################

# caso cauchy


x1<-rcauchy(1000)
hist(x1,prob=TRUE)

aa<-rcauchy(2)
x2<-mean(aa)
x2
# Ahora repetimos 1000 veces. 

matriz2<-matrix(0,nrow=2,ncol=1000)
for(i in 1:1000){ matriz2[,i]<-rcauchy(2)}

xbarra2<-colMeans(matriz2)


hist(xbarra2,prob=TRUE)


matriz5<-matrix(0,nrow=5, ncol=1000)
for(i in 1:1000){ matriz5[,i]<-rcauchy(5)}

xbarra5<-colMeans(matriz5)
hist(xbarra5,prob=TRUE)


matriz30<-matrix(0,nrow=30, ncol=1000)
for(i in 1:1000){ matriz30[,i]<-rcauchy(30)}

xbarra30<-colMeans(matriz30)
hist(xbarra30,prob=TRUE)



matriz500<-matrix(0,nrow=500, ncol=1000)
for(i in 1:1000){ matriz500[,i]<-rcauchy(500)}

xbarra500<-colMeans(matriz500)
hist(xbarra500,prob=TRUE)

matriz1200<-matrix(0,nrow=1200, ncol=1000)
for(i in 1:1000){ matriz1200[,i]<-rcauchy(1200)}

xbarra1200<-colMeans(matriz1200)
hist(xbarra1200,prob=TRUE)


par(mfrow=c(2,3))
hist(x1,probability=T)
hist(xbarra2,probability=T)
hist(xbarra5,probability=T)
hist(xbarra30,probability=T)
hist(xbarra500,probability=T)
hist(xbarra1200,probability=T)


par(mfrow=c(1,1))
boxplot(x1,xbarra2,xbarra5,xbarra30,xbarra500,xbarra1200,
        names=c("n=1", "n=2","n=5", "n=30","n=500","n=1200"))

boxplot(x1,xbarra2,xbarra5,xbarra30,xbarra500,xbarra1200,
        names=c("n=1", "n=2","n=5", "n=30","n=500","n=1200"),
        outline = F)

##################################



# Caso binomial 


x1<-rbinom(1000,1,0.1)
hist(x1,prob=TRUE)

aa<-rbinom(2,1,0.1)
x2<-mean(aa)

# Ahora repetimos 1000 veces. 

matriz2<-matrix(0,nrow=2,ncol=1000)
for(i in 1:1000){ matriz2[,i]<-rbinom(2,1,0.1)}

# aplicamos el promedio por columnas. Obtendremos un vector de 1000 coordenadas
# cada una es una variable aleatoria que es el promedio de dos uniformes (0,1) 
#independientes
xbarra2<-colMeans(matriz2)


hist(xbarra2,prob=TRUE)


matriz5<-matrix(0,nrow=5, ncol=1000)
for(i in 1:1000){ matriz5[,i]<-rbinom(5,1,0.1)}

xbarra5<-colMeans(matriz5)
hist(xbarra5,prob=TRUE)


matriz30<-matrix(0,nrow=30, ncol=1000)
for(i in 1:1000){ matriz30[,i]<-rbinom(30,1,0.1)}

xbarra30<-colMeans(matriz30)
hist(xbarra30,prob=TRUE)
curve(dnorm(x,mean(xbarra30),sd(xbarra30)),add=TRUE)


matriz500<-matrix(0,nrow=500, ncol=1000)
for(i in 1:1000){ matriz500[,i]<-rbinom(500,1,0.1)}

xbarra500<-colMeans(matriz500)
hist(xbarra500,prob=TRUE)

matriz1200<-matrix(0,nrow=1200, ncol=1000)
for(i in 1:1000){ matriz1200[,i]<-rbinom(1200,1,0.1)}

xbarra1200<-colMeans(matriz1200)
hist(xbarra1200,prob=TRUE)



par(mfrow=c(2,3))
hist(x1,probability=T)
hist(xbarra2,probability=T)
hist(xbarra5,probability=T)
hist(xbarra30,probability=T)
hist(xbarra500,probability=T)
hist(xbarra1200,probability=T)

#Mejor aun, pongamos la misma escala en x para comparar

par(mfrow=c(2,3))
hist(x1,probability=T,xlim=c(0,1))
hist(xbarra2,probability=T,xlim=c(0,1))
hist(xbarra5,probability=T,xlim=c(0,1))
hist(xbarra30,probability=T,xlim=c(0,1))
hist(xbarra500,probability=T,xlim=c(0,1))
hist(xbarra1200,probability=T,xlim=c(0,1))

#pongamos tabi?n la misma escala en y

MM<-max(hist(xbarra1200,plot=F)$density)

par(mfrow=c(2,3))
hist(x1,probability=T,xlim=c(0,1),ylim=c(0,MM))
hist(xbarra2,probability=T,xlim=c(0,1),ylim=c(0,MM))
hist(xbarra5,probability=T,xlim=c(0,1) ,ylim=c(0,MM))
hist(xbarra30,probability=T,xlim=c(0,1) ,ylim=c(0,MM))
hist(xbarra500,probability=T,xlim=c(0,1) ,ylim=c(0,MM))
hist(xbarra1200,probability=T,xlim=c(0,1) ,ylim=c(0,MM))

# o bien, hagamos los graficos de estimaciones 
# de la densidad los 6 grupos de datos en el mismo grafico
# density hace un "histograma suavizado" que es mas facil de mirar

par(mfrow=c(1,1))
plot(density(xbarra1200),xlim=c(0,1),lwd=2,main="Densidades estimadas para los 
     promedios de n observaciones U(0,1)")
lines(density(xbarra500),col=2,lwd=2)
lines(density(xbarra30),col=6,lwd=2)
lines(density(xbarra5),col=4,lwd=2)
lines(density(xbarra2),col=5,lwd=2)
lines(density(x1),col=3,lwd=2)
legend( "topright", c("n = 1", "n = 2","n = 5", "n = 30","n = 500","n = 1200"), col = c(3,5,4,6,2,1),lty = rep(1,6), lwd =rep(2,6),cex=0.8)

# para verlos en la misma escala podemos hacer los boxplots
boxplot(x1,xbarra2,xbarra5,xbarra30,xbarra500,xbarra1200,
        names=c("n=1", "n=2","n=5", "n=30","n=500","n=1200"))

#medias muestrales
mean(x1)
mean(xbarra2)
mean(xbarra5)
mean(xbarra30)
mean(xbarra500)
mean(xbarra1200)

#varianzas muestrales

var(x1)
var(xbarra2)
var(xbarra5)
var(xbarra30)
var(xbarra500)
var(xbarra1200)

0.1*0.9
0.1*0.9/2
0.1*0.9/5
0.1*0.9/30
0.1*0.9/500
0.1*0.9/1200

par(mfrow=c(2,3))
qqnorm(x1)
qqline(x1,col="red",lwd=3)
qqnorm(xbarra2)
qqline(xbarra2,col="red",lwd=3)
qqnorm(xbarra5)
qqline(xbarra5,col="red",lwd=3)
qqnorm(xbarra30)
qqline(xbarra30,col="red",lwd=3)
qqnorm(xbarra500)
qqline(xbarra500,col="red",lwd=3)
qqnorm(xbarra1200)
qqline(xbarra1200,col="red",lwd=3)



e1<-(x1-0.1)/sqrt(0.1*0.9)
e2<-(xbarra2-0.1)/sqrt(0.1*0.9/2)
e5<-(xbarra5-0.1)/sqrt(0.1*0.9/5)
e30<-(xbarra30-0.1)/sqrt(0.1*0.9/30)
e500<-(xbarra500-0.1)/sqrt(0.1*0.9/500)
e1200<-(xbarra1200-0.1)/sqrt(0.1*0.9/1200)


par(mfrow=c(2,3))
hist(e1,prob=TRUE)
hist(e2,prob=TRUE)
hist(e5,prob=TRUE)
hist(e30,prob=TRUE)
hist(e500,prob=TRUE)
hist(e1200,prob=TRUE)


par(mfrow=c(1,1))



plot(density(e1200),lwd=2,main="Densidades estimadas para los promedios estandarizados segun TCL
     de n observaciones B(1,0.1)",xlim=c(-4,4))
lines(density(e500),col=2,lwd=2)
lines(density(e30),col=6,lwd=2)
lines(density(e5),col=4,lwd=2)
lines(density(e2),col=5,lwd=2)
lines(density(e1),col=3,lwd=2)
lines(seq(-4,4,by=0.01),dnorm(seq(-4,4,by=0.01)),col=7,lwd=2)
legend( "topright", c("n = 1", "n = 2","n = 5", "n = 30","n = 500","n = 1200","densidad N(0,1)"), col = c(3,5,4,6,2,1,7),lty = rep(1,7), lwd =rep(2,7),cex=0.8)


par(mfrow=c(1,1))
boxplot(e1,e2,e5,e30,e500,e1200, names=c("n=1", "n=2","n=5", "n=30","n=500","n=1200"))
