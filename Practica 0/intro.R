#Introductory Class

# Variables

a <- 10
b <- 50

x <- c(1,2,3,4,5,6)
y <- c(2,5,1,6,8,9)
z <- c(2,4,1)

x*z

# Plot graphic
plot(x,y)
plot(x,x)

(1:10) * x[2]

(1:10)

rep(x, times=2)

help("mtcars")

A <- 1
B <- 2
C <- 3
tratamiento <-c(A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,A,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,B,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C,C)
J <- c(seq(5,100, 5))
J[1]+J[8]

q <- c(3,0,1,6)
r <- c(1,0,2,4)
s <- q*r
s2 <- q%*%r

a <- rbind(q,r)
b <- rbind(q,r)
# c <- a*b
# solve(c) # La matriz no es cuadrada

# Suma de cuadrados
sum((1:100)^2)

which(mtcars[,2] == 4)

#Autos
mtcars[mtcars$disp > 150 & mtcars$mpg > 20]
rownames(mtcars)[mtcars$disp > 150 & mtcars$mpg > 20]

# 4 velocidades y transformacion manual
rownames(mtcars)[mtcars$gear==4 & mtcars$am==1]

# 2 Carburadores
mean(mtcars$mpg[mtcars$carb==2])

#Ej 5
library("readxl") #librería necesaria para levantar datos en formato xls o xlsx

arboles<-read_excel(file.choose())
#a
length(rownames(arboles))
length(colnames(arboles))
names(arboles)

# b
mean(arboles$altura_tot)

#c
sum(arboles$espacio_ve == "ARENALES")

#d
arboles_favoritos<-arboles[arboles$espacio_ve == "ARENALES",]
arboles_favoritos$espacio_ve

#e
arboles$nombre_com
unique(arboles_favoritos$nombre_com)

unique(arboles$nombre_com[arboles$espacio_ve == "ARENALES"])

# EJ 7
help(sample)
sample(10,3,replace=FALSE)

#b
elegir_alumnos <- function(m,n)
{
  sample(1:n,m,replace=FALSE)
}
alumnos<-80
elegir_alumnos(3,alumnos)

#c
set.seed(1)
alumnos_elegidos <- replicate(10, elegir_alumnos(3,alumnos))
alumnos_elegidos

#EJ 8
library('PASWR2')
help("TITANIC3")
prop_clas_1 <-  mean(TITANIC3$survived[TITANIC3$pclass == "1st"])
prop_clas_2 <-  mean(TITANIC3$survived[TITANIC3$pclass == "2nd"])
prop_clas_3 <-  mean(TITANIC3$survived[TITANIC3$pclass == "3rd"])

#b
cuales <- TITANIC3$pclass == "1st" & TITANIC3$sex == "female"
outcome <- TITANIC3$survived
factor1 <- TITANIC3$pclass
factor2 <- TITANIC3$sex

lf1 <- length(levels(factor1))
lf2 <- length(levels(factor2))

prop_de_sobrev <- matrix(0,lf1,lf2)
for(i in 1:lf1)
{
  for(j in 1:lf2)
  {
    prop_de_sobrev[i,j] <- mean(outcome[factor1== levels(factor1)[i]& factor2 == levels(factor2)[j]])
  }
}

colnames(prop_de_sobrev) <- c("female", "male")
rownames(prop_de_sobrev) <- c("1st", "2nd", "3rd")

prop_de_sobrev