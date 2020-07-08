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

