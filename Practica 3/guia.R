#Distro normal
# pnorm trae la probabilidad, el phi(c)
# qnorm trae el valor del percentil, qnorm(p, mu, sigma)
# en tal caso, si queremos el valor para el 90 percentil, buscamos
# qnorm(0.9, 0, 1) y obtenemos el valor mas preciso, caso contrario, buscar
# valores aproximados en la tabla
# Ejercicio 6
#b
pnorm(2.5) - pnorm(-2.5)

#c
1-pnorm(-1.37)

#d
pnorm()

#e
pnorm(1.65) - pnorm(-1.65)

1- pnorm(3.09)

# percentil 0.98 de una distro normal
qnorm(0.98, 0, 1)

#ejercicio 7
#a
pnorm(5.50, 5, 0.5)

#b
pnorm(5.25, 5, 0.5) - pnorm(-5.25, 5, 0.5)

qnorm(0.90, 5, 0.5)

#EJERCICIO 8
pnorm(82, 74.3485, 3.228) - pnorm(78, 74.3485, 3.228)
  
#ejercicio 13
nicoTotalUno <- ((1 - pnorm(2.1, 1.9, 0.4))*0.35) +
((1 - pnorm(2.1, 2.2, 0.3))*0.65)


((1 - pnorm(2.1, 1.9, 0.4))*0.35) / nicoTotalUno

#Ejercicio 16
pexp(1)