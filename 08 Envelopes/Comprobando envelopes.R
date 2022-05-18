# En este script vamos a simular varios glm utilizando la funcion
# simglm del paquete rsq. Esta funcion simula datos en los cuales
# solo la variable x1 fue usada para generar Y, las otras x's son basura.

# Se van a ajustar tres modelos:
# Modelo bueno       1: solo con x1
# Modelo medio malo  2: con x1 y otras x's
# Modelo super malo  3: solo con otras x's

# Poisson -----------------------------------------------------------------
library(rsq)
sdata <- simglm(family="poisson", lambda=4, n=100)
head(sdata$yx)

m1 <- glm(y ~ x.1            , family=poisson, data=sdata$yx)
m2 <- glm(y ~ x.1 + x.2 + x.3, family=poisson, data=sdata$yx)
m3 <- glm(y ~       x.2 + x.3, family=poisson, data=sdata$yx)

# Usando los envelopes de Williams (1987)
# revisar la seccion 1.9.9 pag 65 para entender la construccion
browseURL("https://www.ime.usp.br/~giapaula/texto_2013.pdf")

library(glmtoolbox)

envelope(m1, main='m1')
envelope(m2, main='m2')
envelope(m3, main='m3')

# Gaussian -----------------------------------------------------------------
library(rsq)
sdata <- simglm(family="gaussian", lambda=4, n=100)
head(sdata$yx)

m1 <- glm(y ~ x.1            , family=gaussian, data=sdata$yx)
m2 <- glm(y ~ x.1 + x.2 + x.3, family=gaussian, data=sdata$yx)
m3 <- glm(y ~       x.2 + x.3, family=gaussian, data=sdata$yx)

# Usando los envelopes de Williams (1987)
# revisar la seccion 1.9.9 pag 65 para entender la construccion
browseURL("https://www.ime.usp.br/~giapaula/texto_2013.pdf")

library(glmtoolbox)

envelope(m1, main='m1')
envelope(m2, main='m2')
envelope(m3, main='m3')

# Binomial -----------------------------------------------------------------
library(rsq)
sdata <- simglm(family="binomial", lambda=-2, n=100)
head(sdata$yx)

m1 <- glm(y ~ x.1            , family=binomial, data=sdata$yx)
m2 <- glm(y ~ x.1 + x.2 + x.3, family=binomial, data=sdata$yx)
m3 <- glm(y ~       x.2 + x.3, family=binomial, data=sdata$yx)

# Usando los envelopes de Williams (1987)
# revisar la seccion 1.9.9 pag 65 para entender la construccion
browseURL("https://www.ime.usp.br/~giapaula/texto_2013.pdf")

library(glmtoolbox)

envelope(m1, main='m1')
envelope(m2, main='m2')
envelope(m3, main='m3')

# Gamma -----------------------------------------------------------------
library(rsq)
sdata <- simglm(family="Gamma", lambda=4, n=100)
head(sdata$yx)

m1 <- glm(y ~ x.1            , family=Gamma, data=sdata$yx)
m2 <- glm(y ~ x.1 + x.2 + x.3, family=Gamma, data=sdata$yx)
m3 <- glm(y ~       x.2 + x.3, family=Gamma, data=sdata$yx)

# Usando los envelopes de Williams (1987)
# revisar la seccion 1.9.9 pag 65 para entender la construccion
browseURL("https://www.ime.usp.br/~giapaula/texto_2013.pdf")

library(glmtoolbox)

envelope(m1, main='m1')
envelope(m2, main='m2')
envelope(m3, main='m3')
