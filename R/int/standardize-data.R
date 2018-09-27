#########################
#### STANDARDIZE VARS ###
#########################

rm(list=ls(all=T))
set.seed(1)
dt = read.csv("data/raw/raw-data.csv",as.is = T)

clm.names = names(dt)

vars <- names(dt[, sapply(dt, function(col) length(unique(col))) > 2]) # This identifies non-binary covariates
vars <- vars[vars != c("ChdID","ComID")]

x = match(vars, clm.names)

for (i in 1:length(x)) {
  q=x[i]
  dt[,q] = scale(dt[,q],center=T,scale=T)
}

write.csv(dt,"data/out/StdData.csv",row.names = F)

dt2 = dt[,4:ncol(dt)]

################################
### CREATE INTERACTION TERMS ###
################################

### Once the variables have been standardized, the interaction terms are created by multiplying all covariates by the 'Rainy Season' covariate

dt2 <- dt

vars1 <- c(names(dt2[4:(ncol(dt2)-1)]))  # the -1 is to remove the 'Rainy Season', which would be redundant
res <- numeric()

for (i in 1:length(vars1)){
  q <- as.matrix(dt2[,vars1[i]] * dt2$RainySeason)
  colnames(q) <-(paste(vars1[i],".R",sep=""))
  res <- cbind(res, q)
}

dt3 <- cbind(dt2, res)
dt3 <- dt3[,c(1:3, 32, 4:31, 33:ncol(dt3))] # move rainy season up

write.csv(dt3,"data/out/StdData.csv",row.names = F)
