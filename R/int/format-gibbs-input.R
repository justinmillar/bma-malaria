###################################
###### DESIGNING GIBBS INPUT ######
###################################

rm(list=ls(all=T))
set.seed(1)

input=read.csv("data/out/StdData.csv",as.is=T)
input=input[,!(names(input) %in% c("ChdID")) ]
names(input)

names(input)[1]<-"y"
names(input)[2]<-"village"
q<-names(input)
r<-c("y","village")
vars<-q[!q %in% r]
id<-paste('cov',1:length(vars),sep='')
map=setNames(id,vars)
names(input)=map[as.array(names(input))]

names(input)[1]<-"y"
names(input)[2]<-"village"

CovLookup=cbind(vars,id)

vil<-as.vector(unique(input$village))
head(vil)
length(vil)
num<-seq(1,length(vil))
v.lookup=cbind(vil,num)
input$village2="NULL"

for (i in 1:nrow(v.lookup)){
  print(i)
  index<-which(v.lookup[i,1]==as.character(input$village))
  print(index)
  input$village2[index]=v.lookup[i,2]
}

input$village=input$village2
input$village2=NULL


write.csv(input,file="data/out/GibbsInput.csv", row.names=F)
write.csv(CovLookup,file="data/out/GibbsCovKey.csv",row.names=F)
write.csv(v.lookup,file="data/out/GibbsVilKey.csv",row.names=F)