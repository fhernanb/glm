# -------------------------------------------------------------------------
# Ejemplo 7.1.7 del libro de Foundations of linear and GLM
# de Agresti
# -------------------------------------------------------------------------

url <- "http://users.stat.ufl.edu/~aa/glm/data/Cancer.dat"
Cancer <- read.table(url, header=TRUE)

# Creando la variable logrisktime usado "dplyr"
library(dplyr)
Cancer <- Cancer %>% mutate(logrisktime = log(risktime))

Cancer %>% head()

# Ajustando el modelo
fit <- glm(count ~ factor(histology) + factor(stage) + factor(time),
           data=Cancer, family=poisson(link=log), 
           offset=logrisktime)
summary(fit)

# Envelopes
fit.model <- fit
source("https://www.ime.usp.br/~giapaula/envel_pois")
