# Ejemplo 1 - Gamma -------------------------------------------------------

# Usando los datos de las casas
url <- 'http://users.stat.ufl.edu/~aa/glm/data/Houses.dat'
datos <- read.table(url, header=TRUE)
head(datos, n=5)

# Se desea modelar la variable Price=eta usando un glm con 
# respueta gamma y funcion de enlace inversa

# H0: un modelo gamma con size.
# H1: un modelo gamma con size, new y beds es mejor.

# Se ajustan los modelos
fit0 <- glm(price ~ size,
            data=datos, family=Gamma(link=inverse))
fit1 <- glm(price ~ size + new + beds,
            data=datos, family=Gamma(link=inverse))

# Estimaciones de los parametros para cada modelo
coef(fit0)
MASS::gamma.dispersion(fit0)

coef(fit1)
MASS::gamma.dispersion(fit1)

# Prueba razon de verosimilitud manual
F0 <- (deviance(fit0)-deviance(fit1)) / ((4-2) * 0.1328696)
F0
pf(q=F0, df1=4-2, df2=100-4, lower.tail=FALSE)

# Prueba razon de verosimilitud automatica
anova(fit0, fit1, test="F", dispersion=0.1328696)

# Conclusion: hay evidencias para rechazar H0, un modelo con size,
# new y beds es mejor.


# Ejemplo 2 - Normal ------------------------------------------------------

# Se desea modelar la variable Price=eta usando un glm con 
# respueta normal y funcion de enlace identidad.

# H0: un modelo con EngineSize esta bien.
# H1: un modelo con EngineSize y Type es mejor.

require(MASS)
data("Cars93")
fit0 <- glm(Price ~ EngineSize, data=Cars93)
fit1 <- glm(Price ~ EngineSize + Type, data=Cars93)

# Estimaciones de los parametros para cada modelo
coef(fit0)
summary(fit0)$dispersion

coef(fit1)
summary(fit1)$dispersion

# Prueba razon de verosimilitud manual
F0 <- (deviance(fit0)-deviance(fit1)) / ((7-2) * 49.95815)
F0
pf(q=F0, df1=7-2, df2=100-7, lower.tail=FALSE)

# Prueba razon de verosimilitud automatica
anova(fit0, fit1, test="F", dispersion=49.95815)

# Conclusion: hay evidencias para rechazar H0, un modelo con 
# EngineSize y Type es mejor. Otra forma de decirlo es que la 
# la variable Type aporta informacion al modelo.


# Ejemplo 3 - Inverse Gaussian --------------------------------------------

# Usando los datos de las casas
url <- 'http://users.stat.ufl.edu/~aa/glm/data/Houses.dat'
datos <- read.table(url, header=TRUE)
head(datos, n=5)

# Se desea modelar la variable Price=eta usando un glm con 
# respueta inverse Gaussian y funcion de enlace log

# H0: un modelo con size y taxes.
# H1: un modelo con size, taxes, new y beds es mejor.

# Se ajustan los modelos
fit0 <- glm(price ~ size + taxes,
            data=datos, family=inverse.gaussian(link=log))
fit1 <- glm(price ~ size + new + beds + taxes,
            data=datos, family=inverse.gaussian(link=log))

# Estimaciones de los parametros para cada modelo
coef(fit0)
summary(fit0)$dispersion  # estimacion del parametro de dispersion

coef(fit1)
summary(fit1)$dispersion  # estimacion del parametro de dispersion

# Prueba razon de verosimilitud manual
F0 <- (deviance(fit0)-deviance(fit1)) / ((5-3) * 0.0006811502)
F0
pf(q=F0, df1=5-3, df2=100-5, lower.tail=FALSE)

# Prueba razon de verosimilitud automatica
anova(fit0, fit1, test="F", dispersion=0.0006811502)

# Conclusion: no hay evidencias para rechazar H0, las variables
# new y beds no aportan informacion al modelo.


# Ejemplo 4 - Negative binomial --------------------------------------------

# Usando los datos de las casas
library(GLMsData)
data("hcrabs")
head(hcrabs, n=10)

# Se desea modelar la variable biomasa del follage usando un glm con 
# respueta inverse Gaussian y funcion de enlace log

# H0: un modelo con log(Wt)
# H1: un modelo con log(Wt), Wt y Col es mejor.

# Se ajustan los modelos
library(MASS)
fit0 <- glm.nb(Sat ~ log(Wt), data=hcrabs)
fit1 <- glm.nb(Sat ~ log(Wt) + Wt + Col, data=hcrabs)
fit0 <- glm.convert(fit0)
fit1 <- glm.convert(fit1)

# Estimaciones de los parametros para cada modelo
coef(fit0)
summary(fit0)$dispersion  # estimacion del parametro de dispersion

coef(fit1)
summary(fit1)$dispersion  # estimacion del parametro de dispersion
fit1$theta

# Prueba razon de verosimilitud manual
F0 <- (deviance(fit0)-deviance(fit1)) / ((6-2) * 0.9476757)
F0
pf(q=F0, df1=6-2, df2=173-6, lower.tail=FALSE)

# Prueba razon de verosimilitud automatica
anova(fit0, fit1, test="F", dispersion=0.9476757)

# Conclusion: no hay evidencias para rechazar H0, las variables
# new y beds no aportan informacion al modelo.