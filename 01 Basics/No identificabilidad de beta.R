# Supongamos que tenemos los siguientes datos
# donde la c4 = c2 + c3
X <- matrix(c(1, 2, 5, 7,
              1, 5, 3, 8,
              1, 2, 7, 9), ncol=4, byrow=TRUE)
X

# Simulando la variable y
y <- rnorm(n=3,
           mean = 4 + 2 * X[, 2] - 1 * X[, 3] + 5 * X[, 4], 
           sd=2.5)
y

library(matlib)
showEqn(A=X, b=y)

# Para obtener el rango de X
qr(X)$rank

# Para obtener el vector beta
try(solve(t(X) %*% X) %*% t(X) %*% matrix(y, ncol=1))
try(solve(t(X) %*% X)) # aqui esta el error

# Usando generalizada inversa
library(MASS)
ginv(t(X) %*% X) %*% t(X) %*% matrix(y, ncol=1)

# Usando lm.fit
lm.fit(x=X, y=y)$coefficients

# Usando lm
datos <- as.data.frame(cbind(y, X[, -1]))
mod <- lm(y ~ ., data=datos)
coef(mod)
