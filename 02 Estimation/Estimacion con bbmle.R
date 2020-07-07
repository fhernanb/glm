# En este ejemplo se estiman los parametros de un glm Poison
# usando bblme

# Los datos
y <- c(4, 3, 1)
x <- c(5, 4, 3)

# Funcion de minus log-verosimilitud
minusL <- function(b0, b1) {
  eta <- b0 + b1 * x
  mu <- exp(eta)
  -sum(dpois(x=y, lambda=mu, log=TRUE))
}

library(bbmle)
fit <- mle2(minusL,
            start=list(b0=0, b1=0),
            data=list(x=x, y=y),
            method="Nelder-Mead",
            skip.hessian=FALSE)

summary(fit)

