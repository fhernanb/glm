# Example 4.7
url <- 'http://users.stat.ufl.edu/~aa/glm/data/Houses.dat'
datos <- read.table(url, header=TRUE)
head(datos, n=5)

pairs(datos[c('price','size','taxes')])

# Modelos GAMMA -----------------------------------------------------------
with(datos, plot(density(price)))

fit.gamma <- glm(price ~ size + new + beds + size:new + size:beds,
                 data= datos, 
                 family = Gamma(link = identity))
summary(fit.gamma)
summary(fit.gamma)$coef

fit.g1 <- glm(price ~ size+new+baths+beds, data=datos,
              family=Gamma(link=identity))
fit.g2 <- glm(price~(size+new+baths+beds)^2, data=datos,
              family=Gamma(link=identity))

anova(fit.g1, fit.g2)

summary(fit.g1)
MASS::gamma.dispersion(mod) # para obtener phi
MASS::gamma.shape(mod) # para obtener k

fit.g3 <- glm(price ~ size+new+size:new, data=datos,
              family=Gamma(link=identity))

summary(fit.g3)
MASS::gamma.dispersion(fit.g3) # para obtener phi
MASS::gamma.shape(fit.g3) # para obtener k

par(mfrow=c(2, 2))
plot(fit.g3)

# Este modelo NO esta en el libro pero vale la pena explorarlo
fit.g4 <- glm(price ~ size+new+size:new, data=datos,
              family=Gamma(link="log"))
summary(fit.g4)

# Envelopes
fit.model <- fit.g4
source("https://www.ime.usp.br/~giapaula/envel_gama")
