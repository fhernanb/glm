# En la url de abajo se muestra una pregunta hecha en stackoverflow
# titulada: Selecting variables one by one in glm
# url: https://tinyurl.com/y6zkxhjo
# Vamos a aprender de la pregunta.

x1 <- rnorm(10)
x2 <- rnorm(10)
x3 <- rnorm(10)
y  <- rnorm(10)
x4 <- y + 5 # this will make a nice significant variable to test our code
(mydata <- as.data.frame(cbind(x1,x2,x3,x4,y)))

model <- glm(formula=y~x1+x2+x3+x4,data=mydata)

toselect.x <- summary(model)$coeff[-1,4] < 0.05 # credit to kith

# select sig. variables
relevant.x <- names(toselect.x)[toselect.x == TRUE] 
# formula with only sig variables
sig.formula <- as.formula(paste("y ~", paste(relevant.x, collapse= "+")))

sig.model <- glm(formula=sig.formula,data=mydata)

summary(sig.model)
