# En este ejemplo se solucionan las ecuaciones de verosimilitud
# de un glm Poisson

# Los datos
y <- c(1, 0, 1)
x <- c(4, 3, 3)

# Abajo la función que contiene las ecuaciones iguales a cero
fun <- function(b) {
  b0 <- b[1]
  b1 <- b[2]
  eta <- b0 + b1 * x
  proba <- exp(eta) / (exp(eta) + 1)
  z <- numeric(2) # contiene las ecuaciones
  z[1] <- sum((y-proba) * exp(eta) / (proba * (1-proba) * (exp(eta)+1)^2))
  z[2] <- sum((y-proba) * exp(eta) * x / (proba * (1-proba) * (exp(eta)+1)^2))
  z
}

# Solucionando el sistema
library(nleqslv)
nleqslv(x=c(0, 0), fn=fun, method="Broyden",
        control=list(btol=0.01))

# Solucionando con glm
mod <- glm(y ~ x, family=binomial)
coef(mod)

# El ejemplo anterior funcion muy bien con muchos datos. Por ejemplo
# use los siguientes datos

n <- 500
x <- runif(n=n)
proba <- exp(-2+3*x) / (1+exp(-2+3*x))
y <- rbinom(n=n, size=1, prob=proba)
m <- 1

nleqslv(x=c(0, 0), fn=fun, method="Broyden", control=list(btol=0.01))
mod <- glm(y ~ x, family=binomial)
coef(mod)


# Usando una binomial(n=10, pi) -------------------------------------------
# tomado de https://kkorthauer.org/fungeno2019/methylation/vignettes/1-binomial-regression.html

library(ggplot2)
library(dplyr)

n <- 5
m <- 10
x <- runif(n)
eta <- -0.4 + 0.2 * x
p <- exp(eta) / (1 + exp(eta))
y <- rbinom(n, size=m, prob=p)
cbind(x, p, y)

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
nleqslv(x=c(0, 0), fn=fun, method="Broyden", control=list(btol=0.01))

mod <- glm(cbind(y, m-y) ~ x, family="binomial")
coef(mod)

ggplot(data.frame(x=x,y=as.numeric(y)), 
       aes(x=x, y=y/m)) +
  geom_point(position=position_jitter(height=0.02, width=0.07)) +
  xlab("x") + 
  ylab("y/m") 




