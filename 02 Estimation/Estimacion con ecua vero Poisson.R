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

# Exploremos fun
fun1(betas=c(1, 5))   # upss, queriamos obtener (0, 0)
fun1(betas=c(-1, -5)) # upss, queriamos obtener (0, 0)
fun1(betas=c(-1, 2))  # upss, queriamos obtener (0, 0)

# Ejemplo con 3 datos -----------------------------------------------------

library(nleqslv)
res <- nleqslv(x=c(0, 0), fn=fun1, method="Newton",
               control=list(btol=0.01))
res

# Verificando los resultados de nleqslv

fun1(betas=c(-1.5144535, 0.5951344))

# Lo anterior significa que beta0^ = -1.5144535 y beta1^ = 0.5951344
# en otras palabras, log(mu_i) = -1.51 + 0.60 x
# en otras palabras,     mu_i  = exp(-1.51 + 0.60 x)

# Super Reto --------------------------------------------------------------

# Como se podria modificar la funcion fun1 para que use muestras con mas obs
# pero sin tener que escribir termino a termino en z[1] y z[2]

# Cambiar ___ por la instruccion correcta

y <- c(4, 3, 1)
x <- c(5, 4, 3)

fun2 <- function(betas) {
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


