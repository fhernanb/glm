# Supongamos que tenemos los siguientes datos
barrio <- factor(rep(c("a", "b", "c"), each=3))
barrio

medias <- c(2, 20, 200)
y <- rnorm(n=9, mean=medias[barrio])
y # esto es lo menos importante del ejemplo

# Construyamos la matriz X de dos formas

# La primera
X1 <- cbind(intercepto=1, model.matrix( ~ -1 + barrio))
X1

# Ahora vamos a estimar los parámetros beta
solve(t(X1) %*% X1) %*% t(X1) %*% matrix(y, ncol=1)

# La segunda
X2 <- model.matrix( ~ barrio)
X2

# Ahora vamos a estimar los parámetros beta
solve(t(X2) %*% X2) %*% t(X2) %*% matrix(y, ncol=1)

