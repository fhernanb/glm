# Ejemplo 4.3.2 del libro de Analisis de datos Categoricos
# de Agresti

Marijuana <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Marijuana.dat",
                        header=TRUE)

Marijuana

# Convirtiendo la base de datos en tidy data
library(dplyr)
library(tidyr)

datos <- Marijuana %>% 
  pivot_longer(cols=3:4, names_to="y", values_to="freq") %>% 
  group_by_at(vars(-freq)) %>% 
  expand(temp=1:freq) %>% 
  select(-temp)

datos

# convirtiendo la variable y en factor
datos <- datos %>% 
  mutate(resp = factor(y, levels = c('no', 'yes'))) 

datos

# Modelos 
mod0 <- glm(resp ~ 1, family=binomial, data=datos)
mod1 <- glm(resp ~ gender, family=binomial, data=datos)
mod2 <- glm(resp ~ race, family=binomial, data=datos)
mod3 <- glm(resp ~ gender + race, family=binomial, data=datos)

# Tabla de resumen tradicional
summary(mod3)

# Comparando los modelos usando anova
anova(mod0, mod1, mod2, mod3)

# Comparando los modelos usando Anova
library(car)
Anova(mod3)
