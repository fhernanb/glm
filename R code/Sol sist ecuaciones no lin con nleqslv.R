# El ejemplo de abajo corresponde al ejemplo de la ayuda de la
# funcion nleqslv del paquete nleqslv

fun <- function(x) {
  y <- numeric(2)
  y[1] <- x[1]^2 + x[2]^2 - 2
  y[2] <- exp(x[1]-1) + x[2]^3 - 2
  y
}

# Una prueba
xstart <- c(2, 0.5)
fstart <- fun(xstart)

xstart # el inicio
fstart # valor de fun en el inicio

# a solution is c(1,1)
library(nleqslv)
nleqslv(xstart, fun, control=list(btol=.01))

# comprobando
fun(c(1, 1))

