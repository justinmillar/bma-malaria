#########################
### SPLINE COVARIATES ###
#########################

rm(list=ls(all=T))
set.seed(1)
dt=read.csv("data/raw/raw-data.csv", as.is=T)

n.spline=5 # Set the number of knots/splines + 1 (for the baseline)
incr.spl=1/n.spline # Set the increments for each spline (used for quintile calc)

splines=function(cov,perc){
  # This function is used to create standard quantile splines 
  xmat=matrix(NA,length(cov),n.spline)
  xmat[,1]=cov
  for (i in 2:n.spline){
    tmp=(xmat[,1]-perc[i-1])
    cond=tmp<0
    tmp[cond]=0
    xmat[,i]=tmp
  }
  xmat  
}

# Distance to Health Facility splines
nomes1=c('DistHealth')
res=numeric()
knots=numeric()
for (i in 1:length(nomes1)){
  perc=quantile(dt[,nomes1[i]],seq(from=incr.spl,to=(1-incr.spl),by=incr.spl))
  splined=splines(dt[,nomes1[i]],perc)
  nomes2=c(nomes1[i],paste(nomes1[i],'.s',1:(n.spline-1),sep=''))
  colnames(splined)=nomes2
  res=cbind(res,splined)
  knots=cbind(knots,perc)
}

res=res[, !(colnames(res) %in% nomes1)]
colnames(knots)=nomes1


write.csv(res,"data/out/SplinesOnly-health.csv",row.names=F)
write.csv(knots,"data/out/SplineKnots-health.csv") 
q=cbind(dt,res)

# Distance to Urban Center Splines
nomes1=c('DistUrban')
res=numeric()
knots=numeric()
for (i in 1:length(nomes1)){
  perc=quantile(dt[,nomes1[i]],seq(from=incr.spl,to=(1-incr.spl),by=incr.spl))
  splined=splines(dt[,nomes1[i]],perc)
  nomes2=c(nomes1[i],paste(nomes1[i],'.s',1:(n.spline-1),sep=''))
  colnames(splined)=nomes2
  res=cbind(res,splined)
  knots=cbind(knots,perc)
}

res=res[, !(colnames(res) %in% nomes1)]
colnames(knots)=nomes1

write.csv(res,"data/out/SplinesOnly-urban.csv",row.names=F)
write.csv(knots,"data/out/SplineKnots-urban.csv") 
q=cbind(q,res)

write.csv(q,"data/out/DataSplines.csv",row.names=F)


#########################
#### STANDARDIZE VARS ###
#########################

dt <- q

clm.names = names(dt)

vars <- names(dt[, sapply(dt, function(col) length(unique(col))) > 2]) # This identifies non-binary covariates
vars <- vars[vars != c("ChdID","ComID")]

x = match(vars, clm.names)

for (i in 1:length(x)) {
  q=x[i]
  dt[,q] = scale(dt[,q],center=T,scale=T)
}

dt2 = dt[,4:ncol(dt)]

################################
### CREATE INTERACTION TERMS ###
################################

### Once the variables have been standardized, the interaction terms are created by multiplying all covariates by the 'Rainy Season' covariate
dt2 <- dt
head(dt2)

vars1 <- c(names(dt2[4:(ncol(dt2))]))
vars1 = vars1[vars1 != "RainySeason"]   # the -1 is to remove the 'Rainy Season', which would be redundant
res <- numeric()

for (i in 1:length(vars1)){
  q <- as.matrix(dt2[,vars1[i]] * dt2$RainySeason)
  colnames(q) <-(paste(vars1[i],".R",sep=""))
  res <- cbind(res, q)
}

dt3 <- cbind(dt2, res)
dt3 <- dt3[,c(1:3, 32, 4:31, 33:ncol(dt3))] # move rainy season up


write.csv(dt3,"data/out/StdData.csv",row.names = F)
