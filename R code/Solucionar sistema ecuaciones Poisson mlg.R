# En este ejemplo se solucionan las ecuaciones de verosimilitud
# de un glm Poisson

# Los datos
y <- c(4, 3, 1)
x <- c(5, 4, 3)

# Abajo la función que contiene las ecuaciones iguales a cero
fun <- function(x) {
  b0 <- x[1]
  b1 <- x[2]
  z <- numeric(2) # contiene las ecuaciones
  z[1] <- 4 - exp(b0+5*b1) + 3 - exp(b0+4*b1) + 1 - exp(b0+3*b1)
  z[2] <- (4 - exp(b0+5*b1))*5 + (3 - exp(b0+4*b1))*4 + (1 - exp(b0+3*b1))*3
  z
}

# Solucionando el sistema
library(nleqslv)
nleqslv(x=c(0, 0), fn=fun, control=list(btol=.01))

# Solucionando con glm
mod <- glm(y ~ x, family=poisson)
coef(mod)

