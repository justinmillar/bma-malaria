#########################
#### STANDARDIZE VARS ###
#########################
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
# 
# dt2 = dt[,4:ncol(dt)]
# round(colMeans(dt2),3) # Check to see that only continous variables have mean = 0
