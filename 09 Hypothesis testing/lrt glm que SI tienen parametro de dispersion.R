
# Ejemplo 1 ---------------------------------------------------------------

# Usando los datos de las casas
url <- 'http://users.stat.ufl.edu/~aa/glm/data/Houses.dat'
datos <- read.table(url, header=TRUE)
head(datos, n=5)

# Se desea modelar la variable Price=eta usando dos modelos

# H0: un modelo gamma con size.
# H1: un modelo gamma con size, new y beds es mejor.

# Se ajustan los modelos
fit0 <- glm(price ~ size,
            data=datos, family=Gamma(link=identity))
fit1 <- glm(price ~ size + new + beds,
            data=datos, family=Gamma(link=identity))

# Estimaciones de los parametros para cada modelo
coef(fit0)
coef(fit1)
MASS::gamma.dispersion(fit0)
MASS::gamma.dispersion(fit1)

# Prueba razon de verosimilitud manual
F0 <- ((deviance(fit0) - deviance(fit1)) / (4-2)) / 0.1040076
F0
pf(q=F0, df1=5-3, df2=100-3-1, lower.tail=FALSE)

# Prueba razon de verosimilitud automatica
anova(fit0, fit1, test="F", dispersion=0.1040076)


