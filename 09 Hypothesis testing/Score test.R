# El siguiente ejemplo fue tomado de
# la vineta del paquete enrichwith
# https://cran.r-project.org/web/packages/enrichwith/vignettes/GLMs.html

# Los datos
clotting <- data.frame(conc = c(5,10,15,20,30,40,60,80,100,
                                5,10,15,20,30,40,60,80,100),
                       time = c(118, 58, 42, 35, 27, 25, 21, 19, 18,
                                69, 35, 26, 21, 18, 16, 13, 12, 12),
                       lot = factor(c(rep(1, 9), rep(2, 9))))

# Dibujando los datos y ajustando el modelo de interes
fit0 <- glm(time ~ log(conc), family=Gamma, data=clotting)

fit1 <- glm(time ~ log(conc) * lot, family=Gamma, data=clotting)

# Parametros estimados en fit0
coef(fit0) # betas
MASS::gamma.dispersion(fit0) # Maximum likelihood estimate
summary(fit0)$dispersion # Pearson Estimator
fit0$deviance / fit0$df.residual # Mean Deviance Estimator

# Parametros estimados en fit1
coef(fit1) # betas
MASS::gamma.dispersion(fit1) # Maximum likelihood estimate
summary(fit1)$dispersion # Pearson Estimator
fit1$deviance / fit1$df.residual # Mean Deviance Estimator

# Para enriquecer el modelo glm
library("enrichwith")
enriched_fit1 <- enrich(fit1, with="auxiliary functions")

# Para obtener el vector score
scores_fit1 <- get_score_function(fit1)
scores_fit1()

# Para obtener la matriz de informacion
info_fit1 <- get_information_function(fit1)
info_fit1()

# escudrinar
vcov(fit1)
sqrt(diag(solve(info_fit1())))
summary(fit1)

# Comparando los s.e. con la matriz de Informacion (phi = Pearson)
summary_fit1 <- summary(fit1)
summary_std_errors <- coef(summary_fit1)[, "Std. Error"]
einfo <- info_fit1(dispersion=summary_fit1$dispersion)
all.equal(sqrt(diag(solve(einfo)))[1:4], 
          summary_std_errors, tolerance = 1e-05)

# Creando los vectores con beta y disp bajo H0
beta_hat_0 <- c(-0.01963451, 0.01860894, 0, 0)
disp_hat_0 <- 0.05604664 # Maximum likelihood estimate

# Creando el vector y matriz para la prueba
scores <- scores_fit1(beta_hat_0, disp_hat_0)
info <- info_fit1(beta_hat_0, disp_hat_0)

# El estadistico
score_statistic <- drop(scores%*%solve(info)%*%scores)
score_statistic
# El valor-P
pchisq(score_statistic, 2, lower.tail = FALSE)



