# El presente ejemplo corresponde al ejemplo 1 de la vineta
# del paquete logisticPCA y se puede consultar en:
# https://cran.r-project.org/web/packages/logisticPCA/vignettes/logisticPCA.html

library(logisticPCA)
library(ggplot2)
data("house_votes84")

# Explorando la base de datos
dim(house_votes84)
head(house_votes84)

logsvd_model <- logisticSVD(house_votes84, k = 2)

