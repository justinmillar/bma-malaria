tnorm <- function(n,lo,hi,mu,sig){   #generates truncated normal variates based on cumulative normal distribution
  #normal truncated lo and hi
  
  if(length(lo) == 1 & length(mu) > 1)lo <- rep(lo,length(mu))
  if(length(hi) == 1 & length(mu) > 1)hi <- rep(hi,length(mu))
  
  q1 <- pnorm(lo,mu,sig) #cumulative distribution
  q2 <- pnorm(hi,mu,sig) #cumulative distribution
  
  z <- runif(n,q1,q2)
  z <- qnorm(z,mu,sig)
  z[z == -Inf]  <- lo[z == -Inf]
  z[z == Inf]   <- hi[z == Inf]
  z
}
#------------------------------------
rmvnorm=function (n, mean = rep(0, nrow(sigma)), sigma = diag(length(mean)), 
                  method = c("eigen", "svd", "chol")) 
{
  if (!isSymmetric(sigma, tol = sqrt(.Machine$double.eps), 
                   check.attributes = FALSE)) {
    stop("sigma must be a symmetric matrix")
  }
  if (length(mean) != nrow(sigma)) {
    stop("mean and sigma have non-conforming size")
  }
  sigma1 <- sigma
  dimnames(sigma1) <- NULL
  if (!isTRUE(all.equal(sigma1, t(sigma1)))) {
    warning("sigma is numerically not symmetric")
  }
  method <- match.arg(method)
  if (method == "eigen") {
    ev <- eigen(sigma, symmetric = TRUE)
    if (!all(ev$values >= -sqrt(.Machine$double.eps) * abs(ev$values[1]))) {
      warning("sigma is numerically not positive definite")
    }
    retval <- ev$vectors %*% diag(sqrt(ev$values), length(ev$values)) %*% 
      t(ev$vectors)
  }
  else if (method == "svd") {
    sigsvd <- svd(sigma)
    if (!all(sigsvd$d >= -sqrt(.Machine$double.eps) * abs(sigsvd$d[1]))) {
      warning("sigma is numerically not positive definite")
    }
    retval <- t(sigsvd$v %*% (t(sigsvd$u) * sqrt(sigsvd$d)))
  }
  else if (method == "chol") {
    retval <- chol(sigma, pivot = TRUE)
    o <- order(attr(retval, "pivot"))
    retval <- retval[, o]
  }
  retval <- matrix(rnorm(n * ncol(sigma)), nrow = n) %*% retval
  retval <- sweep(retval, 2, mean, "+")
  colnames(retval) <- names(mean)
  retval
}
#------------------------------------
update.betas=function(param){
  p=ncol(param$cov)
  Sigma.inv=diag(x=1,p)
  Sigma.inv[1,1]=1/1000
  prec=t(param$cov)%*%param$cov+(1/param$sigma2)*Sigma.inv
  var1=solve(prec)
  pmedia=t(param$cov)%*%param$z
  rmvnorm(1,var1%*%pmedia,var1)
}
#------------------------------------
update.sigma2=function(param){
  p=ncol(param$cov)
  a1=(p-1)/2
  
  Sigma.inv=diag(x=1,p)
  Sigma.inv[1,1]=1/1000
  b1=(t(param$betas)%*%Sigma.inv%*%param$betas)/2
  1/rgamma(1,a1,b1)
}
#------------------------------------

update.z=function(param){
  cond=dat$y>0
  media=param$cov%*%param$beta
  res=rep(NA,n)
  res[cond]=tnorm(sum(cond),lo=0,hi=Inf,mu=media[cond],sig=1)
  res[!cond]=tnorm(sum(!cond),lo=-Inf,hi=0,mu=media[!cond],sig=1)
  res
}
#------------------------------------
log.marg.likel=function(cov,z,sig2){
  w=z
  p=ncol(cov)
  Sigma.inv=diag(x=1,p)
  Sigma.inv[1,1]=1/1000
  prec=t(cov)%*%cov+(1/sig2)*Sigma.inv
  var1=solve(prec)
  mu=var1%*%t(cov)%*%w
  -(p/2)*log(sig2)-(1/2)*(-t(mu)%*%prec%*%mu)+(1/2)*determinant(var1)$modulus[1]
}
#------------------------------------
samp.move=function(paramz){
  indin.old=paramz$indin
  p=length(indin.old)
  z=runif(1)	
  p0=1
  if (p == 1) {
    indin.new=birth(paramz$indin,paramz$indout)
    p0=1/3 #death prob 2 -> 1 is (1/3) and birth prob 1 -> 2 is 1. 
  }
  if (p == maxp) {
    if (z < 1/2) {
      indin.new=death(paramz$indin)
      p0=2/3 #birth prob T-1 -> T is (1/3) and death prob T -> T-1 is 1/2
    }
    if (z >= 1/2) indin.new=swap(paramz$indin,paramz$indout)
  }
  if (1 < p & p < maxp) {
    if (z < 1/3) {
      indin.new=birth(paramz$indin,paramz$indout)
      if (p==maxp-1) p0=3/2 #death prob from T -> T-1 is (1/2) and birth prob from T-1 -> T is (1/3)
    }
    if (1/3 < z & z < 2/3) {
      indin.new=death(paramz$indin)
      if (p==2) p0=3 #birth prob from 1 -> 2 is 1 and death prob from 2 -> 1 is 1/3
    }
    if (2/3 < z) indin.new=swap(paramz$indin,paramz$indout)
  }
  pold=log.marg.likel(xmat.orig[,indin.old],paramz$z,paramz$sigma2)
  pnew=log.marg.likel(xmat.orig[,indin.new],paramz$z,paramz$sigma2)+log(p0)
  prob=exp(pnew-pold)
  z=runif(1)
  
  seq1=1:maxp
  k=which(!seq1%in%indin.new)
  indout.new=seq1[k]
  if (z<prob) return(list(xmat=xmat.orig[,indin.new],indin=indin.new,indout=indout.new))
  return(list(xmat=xmat.orig[,indin.old],indin=indin.old,indout=paramz$indout))
}
#------------------------------------------
death=function(indinz){
  if (length(indinz)==3) return(indinz[3]) #cannot delete intercept OR seasonal intercept
  k=sample(3:length(indinz),size=1) 
  indinz[-k]
}
#---------------------------------------------------------------------------------------------------
swap=function(indinz,indoutz){
  if (length(indinz)==3) k=indinz[3] #cannot swap intercept OR seasonal intercept
  if (length(indinz)!=3) k=sample(3:length(indinz),size=1)  
  tmp=indinz[-k]
  include=sample(indoutz,size=1)
  sort(c(tmp,include))
}
#---------------------------------------------------------------------------------------------------
birth=function(indinz,indoutz){
  k=sample(indoutz,size=1)
  sort(c(indinz,k))
}
