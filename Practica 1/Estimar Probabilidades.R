# Se tirran dos dados ¿Cual es la probabilidad de que sumen 8?
# Para estimar la probabilidad podemos tirar dos dados muchas veces y ver que proporcion de veces la suma da 8
# Lo podemos ver como la frecuencia relativa. Pensamos cuanto da la probabilidad si lo pensamos como casos favorables/casos posibles
#P(los dados suman 8) = P({(2,6)},{(3,5)},{(4,4)},{(5,3)},{(6,2)}) / 36 = 5/36

#Comando Sample - Elige numeros al azar con reposicion (replace = TRUE)
# sample(x, n, replace = TRUE)

sample(1:6, 2, replace = TRUE)

#Quiero que se generen siempre los mismos resultados. Fijar semilla
#set.seed(numero) <- Fija los valores
set.seed(56793)
sample(1:6, 2, replace = TRUE)

#Comando replicate -> replicate(n, expr) Repite la expresion de expr n veces y guarda el resultado como matriz
replicate(5, sample(1:6, 2, replace = TRUE))

#help(sample)
#help(replicate)

resultado <- matrix(0, nrow = 2, ncol = 5)
resultado[,1] <- sample(1:6, 2, replace = TRUE)
resultado[,2] <- sample(1:6, 2, replace = TRUE)
resultado[,3] <- sample(1:6, 2, replace = TRUE)
resultado[,4] <- sample(1:6, 2, replace = TRUE)
resultado[,5] <- sample(1:6, 2, replace = TRUE)

resultado

#Otra idea
resultado <- matrix(0, nrow = 2, ncol = 5)
for(i in 1:5){
  resultado[,i] <- sample(1:6, 2, replace = TRUE)
}
resultado

# Como estimar una probabilidad mediante uuna simulacion
# Se tiran dos dados equilibrados ¿Cual es la posibilidad de que sumen 8?
# 1- Simulamos el experimento 1 vez
sum(sample(1:6, 2, replace = TRUE))

#Lo repetimos 100 veces
replicate(100, sum(sample(1:6, 2, replace=TRUE)))

# Nos fijamos cuantas veces o que proporcion de veces da 8
replicate(100, sum(sample(1:6, 2, replace=TRUE)))==8

#Calculamos que proporcion de veces da 8
#mean calcula el promedio, es lo mismo que aplicar la regla de 3 simple
mean(replicate(100, sum(sample(1:6, 2, replace=TRUE)))==8)

set.seed(56793)
mean(replicate(1000, sum(sample(1:6, 2, replace=TRUE)))==8)

set.seed(56793)
mean(replicate(10000, sum(sample(1:6, 2, replace=TRUE)))==8)

set.seed(56793)
mean(replicate(100000, sum(sample(1:6, 2, replace=TRUE)))==8)

#Se puede usar el for
resultado <- rep(NA, 100)
for(i in 1:100){
  resultado[i]<- sum(sample(1:6, 2, replace=TRUE))
}
resultado

mean(resultado == 8)

