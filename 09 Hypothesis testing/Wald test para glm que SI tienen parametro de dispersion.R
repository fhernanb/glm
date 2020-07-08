# Ejemplo 1 - Gamma -------------------------------------------------------

# Usando los datos de las casas
url <- 'http://users.stat.ufl.edu/~aa/glm/data/Houses.dat'
datos <- read.table(url, header=TRUE)
head(datos, n=5)

# Se desea modelar la variable Price=eta usando un glm con 
# respueta gamma, funcion de enlace inversa y size, new y beds.

# El objetivo es estudiar las siguientes hipotesis
# H0: beta_size = -0.000001
# H1: beta_size < -0.000001

# Nota: use como estimador de phi Mean Deviance estimator

# Se ajustan los modelos
fit <- glm(price ~ size + new + beds,
           data=datos, family=Gamma(link=inverse))
summary(fit)

# Mean Deviance estimator for phi
deviance(fit) / df.residual(fit)

# La nueva tabla de resumen es
printCoefmat(coef(summary(fit, dispersion=0.1414655)))

# Calculando el estadistico y su valor-P
T <- (-2.1790e-06 - (-0.000001)) / 2.7644e-07
T
pt(q=T, df=100-4, lower.tail=TRUE) # valor-P

# Conclusion: como el valor-P es menor que un nivel de
# significancia usual, se concluye que SI hay evidencias para
# rechazar H0.

# Ilustrando el valor-P
library(usefultools)
shadow.dist(dist='dt', param=list(df=100-4),
            a=T, type='lower', from=-5, to=-4)

# Conclusion: como el valor-P es menor que 5% hay evidencias
# para rechazar H0, es decir, hay evidencias para pensar
# que beta_size < -0.000001

# Ejemplo 2 - Gamma -------------------------------------------------------

# Usando los datos de las casas
url <- 'http://users.stat.ufl.edu/~aa/glm/data/Houses.dat'
datos <- read.table(url, header=TRUE)
head(datos, n=5)

# Se desea modelar la variable Price=eta usando un glm con 
# respueta gamma, funcion de enlace inversa y size, new y beds.

# El objetivo es estudiar las siguientes hipotesis
# H0: (beta_new, beta_beds) = c(0, 0)
# H1: (beta_new, beta_beds) != c(0, 0)

# Se ajustan los modelos
fit <- glm(price ~ size + new + beds,
           data=datos, family=Gamma(link=inverse))
summary(fit)

# Los betas estimados
coef(fit)
# Todos los betas en matriz columna
beta_hat <- matrix(coef(fit), ncol=1,
                   dimnames=list(names(coef(fit)), NULL))
beta_hat
# Matriz L
L <- matrix(c(0, 0, 1, 0,
              0, 0, 0, 1), ncol=4, byrow=TRUE)
L
L %*% beta_hat # subvector con los betas en H0

# Matriz C
C <- matrix(c(0, 0), ncol=1, byrow=TRUE)
C

# Maximum likelihood estimate
MASS::gamma.dispersion(fit)

# Matriz de varianza y covarianzas
var_cov_hat <- vcov(summary(fit, dispersion=0.1328696))
var_cov_hat

# Estadistico
aux <- L %*% beta_hat - C
F0 <- t(aux) %*% solve(L %*% var_cov_hat %*% t(L)) %*% aux
F0 <- as.numeric(F0)
F0
pf(q=F0, df1=2, df2=100-4, lower.tail=FALSE) # valor-P

# Conclusion: como el valor-P es menor que 5% se concluye que hay 
# evidencias para rechazar H0, es decir, hay evidencias para 
# pensar que (beta_new, beta_beds) != c(0, 0)
