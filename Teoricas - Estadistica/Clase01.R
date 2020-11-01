setwd ("./Teoricas - Estadistica/files/")

departamentos <- read.csv("departamentos-en-venta-2016.csv", header=TRUE)
dim(departamentos)
departamentos[1:10,]

# Datos Pearson
install.packages("UsingR", dependencies=TRUE)

library(UsingR)
#Datos 1o primeras filas
father.son[1:10,]

par(mfrow=c(1,2))
with(data = father.son, hist(fheight, probability = TRUE))
with(data = father.son, hist(sheight, probability = TRUE))

#Boxplot -> boxplot(datos)
boxplot(father.son$fheight, father.son$sheight, names=c("Padres", "Hijos"))

# Grafico de puntos
with(data=father.son, plot(fheight, sheight)) 

#Correlacion
cor(father.son$fheight, father.son$sheight)
# Verificar la formula

#Estimacion por minimos cuadrados, lm -> linear model
ajuste2 <- lm(sheight~fheight, data=father.son)
ajuste2

#Entonces mi formula lineal es 33.8866 + 0.5141X

with(data=father.son, plot(fheight, sheight))
abline(ajuste2$coefficients, col="BLUE")

install.packages("L1pack")
library(L1pack)
ajuste3 <- l1fit(father.son$fheight, father.son$sheight)
abline(ajuste3$coefficients, col=2) 
#Como no hay datos atipicos las rectas son muy parecidas



