# Ejemplo 4.2.3 del libro de Analisis de datos Categoricos
# de Agresti

url <- "http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat"
Crabs <- read.table(url, header=TRUE)
fit <- glm(y ~ width, family=binomial, data=Crabs)

summary(fit)

# Envelopes de Gilberto Alvarenga
# revisar la seccion 1.9.9 pag 65 para entender la construccion
browseURL("https://www.ime.usp.br/~giapaula/texto_2013.pdf")

fit.model <- fit
attach(Crabs)
source("https://www.ime.usp.br/~giapaula/envel_bino")
source("https://www.ime.usp.br/~giapaula/diag_bino")
