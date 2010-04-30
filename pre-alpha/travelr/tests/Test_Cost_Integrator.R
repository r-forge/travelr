library(travelr)

f.calls<-0
env<-environment()
cf<-function(v) {
	assign("f.calls",get("f.calls",env=env)+1,env=env)
	v^2
}
cat("Quadrature...\n")
ff   <- data.frame(a=c(0,0),b=c(0,0))
cong <- data.frame(a=c(2,2),b=c(2,2))
print(cost.integrator(cf,ff,cf(ff),cong,cf(cong),max.depth=5),digits=16)
cat("Calls=",f.calls,"\n")
cat("R integrate\n")
print(integrate(cf,0,2)$value*4,digits=16)
cat("Analytic\n")
print((2^3/3)*4,digits=17)
