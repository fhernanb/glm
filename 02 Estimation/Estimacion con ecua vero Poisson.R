# En este ejemplo se solucionan las ecuaciones de verosimilitud
# de un glm Poisson

# El enunciado de este ejercicio esta en la pag 125 del libro, reto 1

# Equation ----------------------------------------------------------------
fun1 <- function(betas) {
  b0 <- betas[1]
  b1 <- betas[2]
  z <- numeric(2) # contiene LD de las ecuaciones
  z[1] <- (4 - exp(b0+5*b1))   + (3 - exp(b0+4*b1))   + (1 - exp(b0+3*b1))
  z[2] <- (4 - exp(b0+5*b1))*5 + (3 - exp(b0+4*b1))*4 + (1 - exp(b0+3*b1))*3
  z
}

# Busquemos los valores de beta0 y beta1 a ojo
fun1(betas=c(1, 5))   # upss, queriamos obtener (0, 0)
fun1(betas=c(-1, -5)) # upss, queriamos obtener (0, 0)
fun1(betas=c(-1, 2))  # upss, queriamos obtener (0, 0)

# Hagamos al optimizacion de la siguiente manera
library(nleqslv)
res <- nleqslv(x=c(0, 0), fn=fun1, method="Newton",
               control=list(btol=0.01))
res

# Verificando los resultados de nleqslv

fun1(betas=c(-1.5144535, 0.5951344))

# Lo anterior significa que beta0^ = -1.5144535 y beta1^ = 0.5951344
# en otras palabras, log(mu_i)^ = -1.51 + 0.60 x
# en otras palabras,     mu_i^  = exp(-1.51 + 0.60 x)

# Mi funcion de logvero
y <- c(4, 3, 1)
x <- c(5, 4, 3)

my_loglik <- function(beta) {
  b0 <- beta[1]
  b1 <- beta[2]
  eta <- b0 + b1 * x
  mu <- exp(eta)
  sum(dpois(x=y, lambda=mu, log=TRUE))
}

my_loglik(res$x)

# Super Reto --------------------------------------------------------------

# Como se podria modificar la funcion fun1 para que use muestras con
# con muchas observaciones 
# pero sin tener que escribir termino a termino en z[1] y z[2]???

# Su tare consiste en cambiar ___ por la instruccion correcta

y <- c(4, 3, 1)
x <- c(5, 4, 3)

fun2 <- function(betas, x, y) {
  b0 <- betas[1]
  b1 <- betas[2]
  z <- numeric(2)
  mu <- exp(b0 + b1 * x)
  z[1] <- sum( (___ - mu) * ___ )
  z[2] <- sum( (___ - mu) * ___ )
  z
}

library(nleqslv)
res <- nleqslv(x=c(0, 0), fn=fun2, method="Newton",
               control=list(btol=0.01))
res
