# Esta funcion es la modificacion del envelope
# del paquete glmtoolbox

envelope <- function(object, rep=100, conf=0.95, 
                 type=c("quantile","deviance","pearson"), 
                 standardized=FALSE, plot.it=TRUE, identify, ...){
  
  if(object$family$family=="quasi" | object$family$family=="quasipoisson" | object$family$family=="quasibinomial")
    stop("Quasi-likelihood models are not supported!!",call.=FALSE)
  if(any(object$prior.weights == 0)) stop("Only positive weights are supported!!",call.=FALSE)
  
  type <- match.arg(type)
  p <- length(coef(object))
  X <- model.matrix(object)
  mu <- fitted(object)
  n <- length(mu)
  weights <- object$prior.weights
  if(is.null(object$offset)) offs <- rep(0,n) else offs <- object$offset
  
  phi <- summary(object)$dispersion
  phi <- MASS::gamma.dispersion(mod)
  
  ro <- resid(object, type="response")
  phi <- (n-p)/sum((ro/(fitted(object)))^ 2)
  
  rep <- max(1, floor(abs(rep)))
  e <- matrix(0, n, rep)
  
  quantileres <- function(family, y, mu, phi){
    resi <- switch(family,
                   Gamma = pgamma(y, shape=1/phi, scale=mu*phi),
                   inverse.gaussian = pnorm((y/mu-1)/sqrt(y*phi)) + exp(2/(mu*phi))*pnorm(-(y/mu+1)/sqrt(y*phi)),
                   gaussian = pnorm((y-mu)/sqrt(phi)),
                   poisson = ppois(y-1,lambda=mu) + dpois(y,lambda=mu)*runif(length(mu)),
                   binomial = pbinom(y/phi-1,size=1/phi,prob=mu) + dbinom(y/phi,size=1/phi,prob=mu)*runif(length(mu)))
    #resi[resi < 1e-16] <- 1e-16
    #resi[resi > 1-(1e-16)] <- 1-(1e-16)
    return(qnorm(resi))
  }
  
  bar <- txtProgressBar(min=0, max=rep, initial=0, width=min(50,rep), char="+", style=3)
  
  i <- 1
  
  while(i <= rep) {
    
    if(object$family$family=="inverse.gaussian"){
      w <- rchisq(n,df=1)
      u <- mu + mu^2*w*phi/2 - (mu*phi/2)*sqrt(4*mu*w/phi + mu^2*w^2)
    }
    
    resp <- switch(object$family$family,
                   Gamma = rgamma(n, shape=object$prior.weights/phi, scale=mu*phi/object$prior.weights),
                   inverse.gaussian = ifelse(runif(n)<=mu/(mu+u),u,mu^2/u),
                   gaussian = sqrt(phi/object$prior.weights)*rnorm(n) + mu,
                   poisson = rpois(n,lambda=mu),
                   binomial = rbinom(n,size=object$prior.weights,prob=mu)/object$prior.weights)
    
    fits <- try(glm.fit(x=X, y=resp, family=object$family,
                        offset=offs,start=coef(object),
                        weights=weights),
                silent=TRUE)
    
    if(is.list(fits)){
      if(fits$converged==TRUE){
        phis <- summary((resp-fits$fitted.values)^2*weights/object$family$variance(fits$fitted.values))/fits$df.residual
        if(object$family$family=="binomial" | object$family$family=="poisson") phis <- 1
        if(type=="quantile")
          rs <- quantileres(object$family$family,resp,fits$fitted.values,phis/weights)
        if(type=="deviance"){
          rs <- sqrt(object$family$dev.resids(resp,fits$fitted.values,weights)/sqrt(phis))
          rs <- ifelse(resp >= fits$fitted.values,1,-1)*rs
        }
        if(type=="pearson")
          rs <- (resp-fits$fitted.values)*sqrt(weights/object$family$variance(fits$fitted.values)*phis)
        if(standardized){
          Xw <- X*matrix(sqrt(fits$weights),n,p)
          salida <- svd(Xw)
          h <- apply(salida$u^2,1,sum)
          rs <- rs/sqrt(1-h)
        }
        e[,i] <- sort(rs)
        setTxtProgressBar(bar,i)
        i <- i + 1
      }
    }
  }
  close(bar)
  alpha <- 1 - max(0,min(1,abs(conf)))
  e <- as.matrix(e[,1:(i-1)])
  es <- apply(e,1,function(x) return(quantile(x,probs=c(alpha/2,0.5,1-alpha/2))))
  rd <- if(type=="quantile")
    rd <- quantileres(object$family$family,object$y,mu,phi/object$prior.weights)
  else
    rd <- residuals(object,type=type)/sqrt(phi)
  if(standardized){
    Xw <- X*matrix(sqrt(object$weights),n,p)
    salida <- svd(Xw)
    h <- apply(salida$u^2,1,sum)
    rd <- rd/sqrt(1-h)
  }
  if(plot.it){
    rango <- 1.1*range(rd)
    oldpar <- par(no.readonly=TRUE)
    on.exit(par(oldpar))
    par(pty="s")
    qqnorm(es[2,],axes=FALSE,xlab="",ylab="",main="", type="l",ylim=rango,lty=3)
    par(new=TRUE)
    qqnorm(es[1,],axes=FALSE,xlab="",ylab="",main="", type="l",ylim=rango,lty=1)
    par(new=TRUE)
    qqnorm(es[3,],axes=FALSE,xlab="",ylab="", main="", type="l",ylim=rango,lty=1)
    par(new=TRUE)
    nano <- list(...)
    nano$y <- rd
    nano$type <- "p"
    nano$ylim <- rango
    if(is.null(nano$labels)) labels <- 1:nrow(X)
    else labels <- nano$labels
    if(is.null(nano$pch)) nano$pch <- 20
    if(is.null(nano$col)) nano$col <- "black"
    if(is.null(nano$xlab)) nano$xlab <- "Expected quantiles"
    if(is.null(nano$ylab)) nano$ylab <- "Observed quantiles"
    if(is.null(nano$main)) nano$main <- paste0("Normal QQ plot with simulated envelope\n of ",type,"-type residuals")
    outm <- do.call("qqnorm",nano)
    if(!missingArg(identify)){
      identify(outm$x,outm$y,labels=labels,n=max(1,floor(abs(identify))))
    }
  }
  out_ <- cbind(t(es),rd)
  colnames(out_) <- c("Lower limit","Median","Upper limit","Residuals")
  return(invisible(out_))
}


