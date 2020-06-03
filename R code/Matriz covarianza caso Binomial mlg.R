# En este ejemplo se solucionan las ecuaciones de verosimilitud
# y se calcula la matriz de covarianza de beta hat

# Usando una binomial(n=10, pi) -------------------------------------------
# tomado de https://kkorthauer.org/fungeno2019/methylation/vignettes/1-binomial-regression.html

n <- 5
m <- 10
x <- runif(n)
eta <- -0.4 + 0.2 * x
p <- exp(eta) / (1 + exp(eta))
y <- rbinom(n, size=m, prob=p)

fun <- function(b) {
  b0 <- b[1]
  b1 <- b[2]
  eta <- b0 + b1 * x
  proba <- exp(eta) / (1 + exp(eta))
  z <- numeric(2) # contiene las ecuaciones
  z[1] <- sum(m * (y/m-proba) * exp(eta) / (proba * (1-proba) * (exp(eta)+1)^2))
  z[2] <- sum(m * (y/m-proba) * exp(eta) * x / (proba * (1-proba) * (exp(eta)+1)^2))
  z
}

# lo siguiente no me funciona
library(nleqslv)
res <- nleqslv(x=c(0, 0), fn=fun, method="Broyden", 
               control=list(btol=0.01))

res$x # las estimaciones

# Para encontrar la matriz de covarianzas de beta hat
eta <- res$x[1] + res$x[2] * x
proba <- exp(eta) / (1 + exp(eta))
wi <- (exp(eta) / (exp(eta)+1)^2)^2 / (proba * (1-proba) / m)
W_hat <- diag(wi)
X <- model.matrix(y ~ x)
solve(t(X) %*% W_hat %*% X)

mod <- glm(cbind(y, m-y) ~ x, family="binomial")
vcov(mod)




