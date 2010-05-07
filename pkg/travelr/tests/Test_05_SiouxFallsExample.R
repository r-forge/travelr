# A "three-step" model (no mode choice) with trip distribution via gravity model 
library(travelr)
# load("data/SiouxFallsAset.Rdata")
data(SiouxFalls)

# Trip Generation
str(SiouxFalls.net)
productions<-rowSums(SiouxFalls.od)
attractions<-colSums(SiouxFalls.od)
print(data.frame(P=productions,A=attractions))
cat("Total productions:",sum(productions),"\n")
cat("Total attractions:",sum(attractions),"\n")

# Highway Skims
cost.function<-with(SiouxFalls.net$Links,function(...)FFTime)
aclass <- make.assignment.class(SiouxFalls.net,"All",SiouxFalls.od)
aset <- new.assignment.set(SiouxFalls.net,list(All=aclass),cost.volume.type="vector",cost.function=cost.function)
paths <- build.paths(aset,aset$ff.cost)
travel.times <- skim.paths(paths,aset$ff.cost)[["All"]] # only one purpose: "All trips"

# Trip Distribution (Gravity Model with gamma function)
options(width=180)
base.distribution <- hwy.gamma.function(travel.times,-0.02,-0.123) # HBW coefficients from NCHRP 365
trip.table <- ipf(base.distribution,list(rows=productions, cols=attractions),method="absolute")

# Note that either of the following is possible
# The function version will give nicer error messages
aset[[c("classes","All","demand")]]<-trip.table
aset <- hwy.update.demand(aset,"All",trip.table)

# Trip Assignment
assignment.results <- highway.assign(aset,method="Frank.Wolfe")
loaded.links <- assignment.results$volumes
print(loaded.links)
