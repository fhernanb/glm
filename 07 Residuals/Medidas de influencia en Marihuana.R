# Ejemplo 4.3.2 del libro de Analisis de datos Categoricos
# de Agresti

Marijuana <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Marijuana.dat",
                        header=TRUE)

Marijuana

# Convirtiendo la base de datos en tidy data
library(dplyr)
library(tidyr)

datos <- Marijuana %>% 
  pivot_longer(cols=3:4, names_to="y", values_to = "freq") %>% 
  group_by_at(vars(-freq)) %>% 
  expand(temp = 1:freq) %>% 
  select(-temp)

datos

# convirtiendo la variable y en factor
datos <- datos %>% 
  mutate(resp = factor(y, levels = c('no', 'yes'))) 

datos

# Modelo
mod <- glm(resp ~ gender + race, family=binomial, data=datos)

# Medidas de influencia
dfbeta(mod)
dfbetas(mod)
dffits(mod)

# Graficos utiles
library(car)
residualPlots(mod)
dfbetaPlots(mod)
dfbetasPlots(mod)
influencePlot(mod)

qqPlot(rstudent(mod))
qqPlot(rstandard(mod))

# Usando los envelopes de Williams (1987)
# revisar la seccion 1.9.9 pag 65 para entender la construccion
browseURL("https://www.ime.usp.br/~giapaula/texto_2013.pdf")

library(glmtoolbox)
envelope(mod)

# Quantile residuals
library(statmod)
qr <- qresiduals(mod)
car::qqPlot(qr, distribution="norm")

