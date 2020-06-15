# 6.1.2 Example: What Do Alligators Eat?
# AN INTRODUCTION TO CATEGORICAL DATA ANALYSIS
# Agresti

url <- 'http://www.stat.ufl.edu/~aa/cat/data/Alligators.dat'
Gators <- read.table(url, header=TRUE)
Gators$y <- factor(Gators$y, levels=c('O', 'F', 'I')) 

# Exploring the data
table(Gators$y)
barplot(table(Gators$y))
with(Gators, boxplot(x ~ y, las=1, ylab='Length (mt)'))

# Fitting the model
library(VGAM) 
mod <- vglm(y ~ x, family=multinomial(refLevel="O"), data=Gators) 
summary(mod)

# Exploring the object mod
class(mod)

# Las probabilidades
mod@fitted.values

# Predicting the class for alligator with x=3.89
predict(mod, newdata=data.frame(x=3.89), type='response')

predict(mod, newdata=data.frame(x=3.89), type="link")
predict(mod, newdata=data.frame(x=3.89), type="response")
predict(mod, newdata=data.frame(x=3.89), type="terms")

# para predecir la clase
w <- predict(mod, newdata=data.frame(x=3.89), type='response')
clase <- nnet::which.is.max(w)
levels(Gators$y)[clase]

# My confusion table
w <- predict(mod, type='response')
clase <- apply(w, 1, nnet::which.is.max)
y_hat <- levels(Gators$y)[clase]
y_hat <- factor(y_hat, levels=c('O', 'F', 'I'))
tabla <- table(clasificacion=y_hat, real=Gators$y)
tabla

# Using caret
library(caret)
confusionMatrix(data=y_hat, reference=Gators$y)

