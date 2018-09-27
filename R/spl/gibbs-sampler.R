###########################
###### GIBBS SAMPLER ######
###########################


rm(list=ls(all=T))
set.seed(1)
source('R/spl/functions-gibbs.R')
dat=read.csv('data/out/GibbsInput.csv',as.is=T)

nvillage=length(unique(dat$village))
npeople.village=table(dat$village)
n=nrow(dat)

ind=grep('cov',colnames(dat))
xmat.orig=cov=data.matrix(cbind(1,dat[,ind]))
maxp=ncol(cov)

#initial values
betas=c(0.2,rep(0,maxp-1))
sigma2=1
cond=dat$y>0
z=ifelse(cond,runif(n),runif(n,min=-1,max=0))
indin=1:10
indout=11:maxp
param=list(z=z,sigma2=sigma2,betas=matrix(betas[indin],length(indin),1),
           indin=indin,indout=indout,cov=cov[,indin])

ngibbs=10000
vec.betas=matrix(NA,ngibbs,maxp)
vec.outros=matrix(NA,ngibbs,1)

for (i in 1:ngibbs){
  print(c(i,param$indin))
  if (!1%in%param$indin) break;
  tmp=samp.move(param)
  param$cov=tmp$xmat
  param$indin=tmp$indin
  param$indout=tmp$indout
  
  param$betas=t(update.betas(param))
  
  param$sigma2=update.sigma2(param)
  param$z=update.z(param)
  
  tmp=rep(0,maxp)
  tmp[param$indin]=param$betas
  vec.betas[i,]=tmp
  vec.outros[i,]=param$sigma2
  
}

write.csv(vec.betas,'data/out/gibbs_betas.csv',row.names=F)
write.csv(vec.outros,'data/out/gibbs_others.csv',row.names=F)