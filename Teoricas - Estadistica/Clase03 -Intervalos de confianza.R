# Ejemplo de intervalos de confianza
"Se quiere estimar la estatura media de los estudiantes de computacion en base a la estatura
de n alumnos, asumiendo que la estatura sigue una distribucion N(mu, sigma^2) dar el estimador
insesgado de mu. Como podria estimar la precisison de esta estimacion?"

# Ejercicio 8 de la practica 7
# Simulacion
"Vamos a comparar los estimadores de theta en U(0, theta)
Momentos: 2*XRaya
MV: max"

#Pruebas
x <- runif(10, 0, 1) # Theta = 1
x
max(x)
# Aceptable: No me dice nada de si es bueno o malo el maximo

"Deberia haber hecho bien las cuentas, la media me da otro numero
Tengo otro dato, que puedo hacer?"
2*mean(x)
# Tengo poca estructura, los valores estan volando

"Que deberia graficar?
Busco explicar Y
Recta de regresion? Y = recta(X)
Y en funcion de lo que explica. En este caso son pares (X,Y)
Pensar: Tiene sentido los graficos? Que graficos? Una recta de regresion?
Porque ese grafico?
Si hago un histograma o un boxplot...tendra que ver con lo que busco?
Yo busco que el histograma sea constante, en este caso no tiene nada de constante
Tenemos una sola muestra...
Tiro un n mas grande, n = 100
Busco mis estimadores"
x <- runif(100, 0, 1)
x
max(x)
plot(x)
hist(x)
"Ahora el maximo me va con 0.9915951, un mejor valor que el anterior
Yo esperaria que el numero me acerque a 1, o al numero que yo buscaria en realidad
Ahora estoy buscando el 1 pero porque lo declare, en la practica uno quiere estimar
algo desconocido y tiro valores para estimar que tan bueno son los estimadores, ponele"
2*mean(x)
"Todo lo que busco, se basa en el supuesto de la normalidad, ¿Como lo demuestro?
- Hay un proceso que hace que los datos sean normal?
Hawkoski -> Libro de errores (Del DC)
Teorema central del limite -> Me garantiza -> En el infinito, los promedios son una normal"
plot(x)
"Ahora tiene pinta de uniforme"
hist(x)
"No estamos mirando nada de los estimadores
Si voy aumentando el N, voy viendo resultados mas lindos
Me estan dando valores cercanos a la teorica que deberian ser 1 como buscabamos
Uno de los valores tiende a ser menor al otro
¿Sera que hay estimadores insesgados? ¿Asintoticamente Insesgados?
Si fueran asintoticamente insesgados ¿Alguno tiene mayor variabilidad?"

# Simulacion
"Porque hay 2 n? n de tamaño fijo y N que supongo esa estimulacion 
donde armamos todo y revisamos lo que queriamos lo voy a repetir muchas veces
n: datos
N: repeticiones"
n <- 100
N <- 1000


estim_boot1 <- rep(0, N)
estim_boot2 <- rep(0, N)

"Ahora me convenzo empiricamente de lo que hice
Guardo las estimaciones de los valores"
for(i in 1:N){
  x_i <- runif(n, 0, 1)
  estim_boot1[i] <- max(x_i) #MV: maximo
  estim_boot2[i] <- 2*mean(x_i) #Momentos: 2xraya
}
"Hago histogramas de lo que obtuve, de los estimadores. El estimador
es una v.a. la cual le puedo calcular las cosas y muchas veces no sabemos
que distribucion tiene
Yo estoy con unas muestras, deberia dar cosas esperadas intutivamente como normales
Tener cuidado con ver los ejes"
hist(estim_boot1)
"Ahora este estim_boot2 tiene colas a ambos lados
Porque tiene una forma tan simetrica? Simulacion : 1000 y el TCL>
Es curioso pues las Xi son uniformes
Galton -> Propone el modelo de regresion"
hist(estim_boot2)

mean(estim_boot1)
mean(estim_boot2)

"Una forma de aproximar el error cuadratico medio es con esta cuenta"
ECM1 <- sum((estim_boot1-1)^2)/N
ECM2 <- sum((estim_boot2-1)^2)/N

"Veo que uno de los dos es mas chico
El ECM1 es mas chico que el de Momentos (EMC2) pese a que ambos
son consistentes. A veces no es tanto que sean consistentes si no saber a que tasa
converge esa varianza
Yo me quedo con el que tenga menor error cuadratico medio"
ECM1
ECM2
