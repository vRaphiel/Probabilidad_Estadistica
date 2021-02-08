# Prediccion

# La mediana de una exponencial de parametro lambda = 1
# qexp calcula la inversa, es decir, el valor para el cual se cumple el cuartil buscado
qexp(0.5, 1)

# Ejemplo de ECM. Genero una distribucion de exponenciales
y <- rexp(100,1)
plot(y)
abline(h=1)
abline(h=0.69)
text(95, 1.1, "y=1", col="blue")
text(95, 0.8, "y=0.69", col="blue")

# Supongamos que el ingreso total familiar en argentina sigue una distribucion gamma(1.33, 1/30901)
# Calcular el mejor predictor del ITF segun el criterio del ECM y el EAM

mediana <- qgamma(0.5, shape=1.33, rate=1/30901)
mediana

media <- 1.33 * 30901
media

# Teniendo datos con las estaturas de padres e hijos.
predecir_estatura <- function(estatura_padre){
  0.53*estatura_padre + 33.045
}

estatura_padre <- seq(60,80)
plot(estatura_padre, predecir_estatura(estatura_padre), type="l",xlab="estatura  padre", ylab="estatura hijo")

# Datos de pearson
library(UsingR)
with(data=father.son, plot(fheight, sheight))

# correlacion de los parametros buscados
cor(father.son$fheight, father.son$sheight)
# Es equivalente con la formula de estimacion

# La ordenada al origen en el predictor es alpha, y la pendiente es beta
# Se pueden calcular los parametros ordenada y pendiente con lm -> linear model

#La recta de regresion estimada de PARAMETRO1 vs PARAMETRO2 es
# lm(formula = PARAMETRO1 ~ PARAMETRO2, data = dataset)
# Respuesta: el primer parametro (intercept) es la ordenada, el segundo la pendiente

ajuste2 <- lm(sheight~fheight, data=father.son)
ajuste2
abline(ajuste2$coefficients)