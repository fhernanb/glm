# Los ejemplos aqui mostrados fueron tomados de la vineta del
# paquete matlib https://cran.r-project.org/web/packages/matlib 


# Ejemplo 1 ---------------------------------------------------------------
# Encontrar los valores de x1 y x2 que solucionan el sistema:

## 1*x1 - 1*x2  =  2 
## 2*x1 + 2*x2  =  1

library(matlib)

A <- matrix(c(1, 2, -1, 2), ncol=2, nrow=2)
b <- c(2,1)
showEqn(A, b)

# Plot the equations:
plotEqn(A, b)

# Para encontrar la solucion
Solve(A, b, fractions = TRUE)
