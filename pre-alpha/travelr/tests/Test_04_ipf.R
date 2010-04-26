suppressPackageStartupMessages(library(travelr))

f<-list(rows=c(400,460,400,702),cols=c(260,400,500,802))
m<-matrix(c(5,50,100,200,50,5,100,300,50,100,5,100,100,200,250,20),nrow=4,ncol=4,byrow=TRUE)
dimnames(m)<-list(Rows=c("R1","R2","R3","R4"),Cols=c("C1","C2","C3","C4"))

cat("Growth Factors:\n")
for ( n in names(f) ) cat(n,":",paste(f[[n]],collapse=","),"\n")

cat("Before:\n")
print(addmargins(m))
m.new<-ipf(m,f,method="absolute",max.rmse=1e-10,max.iter=3)
cat("After:\n")
print(m.new,digits=4)
# print(addmargins(m.new),digits=3)
# cat("RMSE:",attributes(m.new)$RMSE,"\n")
# cat("Iterations:",attributes(m.new)$Iteration,"\n\n")

cat("Before:\n")
print(addmargins(m))
m.new<-ipf(m,f,method="absolute",max.rmse=1e-10,max.iter=50)
cat("After:\n")
print(m.new,digits=4)

m2<-matrix(c(0.74,0.33,0.17,0.11,0.30,0.74,0.30,0.15,0.21,0.27,0.61,0.50,0.09,0.17,0.45,0.61),nrow=4,ncol=4,byrow=TRUE)
dimnames(m2)<-list(Rows=c("R1","R2","R3","R4"),Cols=c("C1","C2","C3","C4"))

cat("Before:\n")
print(addmargins(m2))
m2.new<-ipf(m2,f,method="absolute",max.rmse=1e-10,max.iter=3)
cat("After:\n")
print(m2.new,digits=4)

cat("Before:\n")
print(addmargins(m2))
m2.new<-ipf(m2,f,method="absolute",max.rmse=1e-10,max.iter=50)
cat("After:\n")
print(m2.new,digits=4)
