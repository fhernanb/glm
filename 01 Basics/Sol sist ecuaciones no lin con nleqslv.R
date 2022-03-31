
# Ejemplo 1 ---------------------------------------------------------------

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


# Ejemplo 2 ---------------------------------------------------------------

# El ejemplo de abajo corresponde al ejemplo mostrado en
# https://math.libretexts.org/Bookshelves/Algebra/Intermediate_Algebra_(OpenStax)/11%3A_Conics/11.06%3A_Solving_Systems_of_Nonlinear_Equations

fun <- function(x) {
  y <- numeric(2)
  y[1] <-  x[1]   - x[2] + 2
  y[2] <- -x[1]^2 + x[2]
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
fun(c(2, 4))

# Otra prueba iniciando de los valores x1=-3 y x2=1
xstart <- c(-3, 1)
fun(xstart)

# a solution is c(1,1)
library(nleqslv)
nleqslv(x=xstart, 
        fn=fun, 
        method="Broyden",
        control=list(btol=0.01))

# comprobando
fun(c(-1, 1))

