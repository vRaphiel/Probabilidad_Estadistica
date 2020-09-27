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
