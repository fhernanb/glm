# En este ejemplo se solucionan las ecuaciones de verosimilitud
# y se calcula la matriz de covarianza de beta hat

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
res <- nleqslv(x=c(0, 0), fn=fun, method="Newton",
               control=list(btol=0.01))

res$x # las estimaciones

# Para encontrar la matriz de covarianzas de beta hat
eta <- res$x[1] + res$x[2] * x
wi <- exp(eta)
W_hat <- diag(wi)
X <- model.matrix(y ~ x)
solve(t(X) %*% W_hat %*% X)

# Solucionando con glm
mod <- glm(y ~ x, family=poisson)
vcov(mod)

