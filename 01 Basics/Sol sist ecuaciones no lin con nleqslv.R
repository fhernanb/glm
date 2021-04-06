# El ejemplo de abajo corresponde al ejemplo de la ayuda de la
# funcion nleqslv del paquete nleqslv

# fun es una funcion a la que le ingresa un vector x de dos
# elementos y ella entrega un vector y de dos elementos.

fun <- function(x) {
  y <- numeric(2)
  y[1] <- x[1]^2 + x[2]^2 - 2
  y[2] <- exp(x[1]-1) + x[2]^3 - 2
  y
}

# Una prueba iniciando de los valores x1=2 y x2=0.5
xstart <- c(2, 0.5)
fun(xstart)

# a solution is c(1,1)
library(nleqslv)
nleqslv(x=xstart, 
        fn=fun, 
        method="Broyden",
        control=list(btol=0.01))

# comprobando
fun(c(1, 1))

