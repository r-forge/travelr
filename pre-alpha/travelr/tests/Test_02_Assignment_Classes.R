suppressPackageStartupMessages(library(travelr))

# Load SiouxFalls network

load("data/SiouxFalls.rda")
stopifnot(exists("SiouxFalls.net"))
stopifnot(exists("SiouxFalls.od"))

# Construct cost function based on no-toll TNTP cost equation
# Build assignment set ("single"/default)
cost.sf<-build.BPR.function( with(SiouxFalls.net$Links,
                                 data.frame( TIME=FFTime,
											 CAPACITY=Capacity,
											 ALPHA=B,
											 BETA=Power)) )
aclass.sf <- make.assignment.class(SiouxFalls.net,"All",SiouxFalls.od,cost.function=cost.sf)
classes.sf<-vector("list",0)
classes.sf<-add.assignment.class(classes.sf,aclass.sf)
cat(paste(rep("=",80),collapse=""),"\n")
cat("Structure of Sioux Falls Assignment Class (aclass.sf)\n")
str(aclass.sf)
cat("Building 'single' assignment set.\n")
aset.sf <- new.assignment.set(SiouxFalls.net, classes.sf, assignment.type="single")
cat(paste(rep("=",80),collapse=""),"\n")
cat("Structure of Sioux Falls Assignment Set (aset.sf)")
str(aset.sf)

# Load Richmond network
# Construct assignment set for Richmond network ("total")
# Use two classes, drop toll facilities, construct "non-toll" matrix

# Construct assignment set for Richmond network ("multi")
# Use three classes, subdividing a "Truck" class from the "non-toll" matrix
# The truck class gets the same cost function as the single (i.e. all vehicles added up)
# The other two classes get a cost function with PCE equivalent applied to trucks

# Save out an .Rdata file containing:
#    SiouxFalls network, assignment set
#    Richmond network, assignment set (total), assignment set (multi)
save(SiouxFalls.net,SiouxFalls.od,aset.sf,file="data/SiouxFallsAset.Rdata")

# That should be all we need for further testing (Test_03_Assignment)
# This code will become part of the basic vignettes showing how to manipulate
#   matrices and perform highway assignment

