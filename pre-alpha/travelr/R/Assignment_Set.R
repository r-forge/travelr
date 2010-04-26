####################################################################################################
#   travelr\r\Assignment_Set.R by Jeremy Raw  Copyright (C) 2010
# 
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
# 
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
# 
#   A copy of the GNU General Public License is available at
#   http://www.r-project.org/Licenses/
#   and included in the R distribution (in directory ‘share/licenses’).
####################################################################################################

# This file defines the Assignment Set mechanism for handling single and multi-class assignment
# and presenting an abstract interface for assignment algorithms.

# The assignment.set structures define the network and demand that will be fitted to each other
# during the assignment process.  The basic operations used in the assignment algorithms are
# the following (implemented as generic functions):
#   1. cost.function (uses assignment.set cost.function; generates cost.set from volume.set)
#   2. build.paths (uses assignment.set network.set; generates path.set from cost.set)
#   3. load.demand (uses assignment.set demands; generates volume.set from path.set)


# free.flow will produce a volume set of zero flow for use in computing free-flow costs

free.flow<-function(aset)
	as.data.frame(
	lapply(aset$classes,
		   FUN=function(aclass,numLinks) vector("numeric",numLinks),
		   numLinks=aset$network$numLinks)
	)

# build.paths will produce a list of path sets from route costs

build.paths<-function(aset,costs)
	mapply(function(ac,cost) .shortest.paths(ac$network,cost),
	       aset$classes,costs,
		   USE.NAMES=TRUE, # Keep assignment class names on resulting list of path structures
		   SIMPLIFY=FALSE) # and leave results as a list

# load.paths will map demand matrices onto path sets

load.paths<-function(aset,paths)
	as.data.frame(mapply(function(ac,paths) .load.paths(paths,ac$demand),
			      aset$classes, paths,
			      USE.NAMES=TRUE, # Keep assignment class names on resulting volume set list
			      SIMPLIFY=FALSE) # and leave the results as a list
			     )

# build.and.load.paths provides a slight efficiency boost by leaving more of the
# logic at C-level.  It's useful in link-based assignment, but not in path-based assignment
#    where one is equilibrating path sets, rather than volume sets
build.and.load.paths<-function(aset,costs) {
	result=mapply( function(ac,cost).build.and.load.paths(ac$network,cost,ac$demand),
				   aset$classes,costs,
				   USE.NAMES=TRUE, # Keep assignment class names on resulting volume set list
				   SIMPLIFY=FALSE) # and leave results as a list (each element of which is
				                   # a list with "paths" and "volumes" elements)
	paths = lapply(result,function(p)p$paths)      # Paths for all classes in one list
	volumes = lapply(result,function(p)p$volumes)  # Volumes for all classes in one list
	return( list(paths=paths, volumes=volumes, result=result) )
}

# intercept.paths returns a list of OD matrices ("od") whose paths intercept selected links
# we get one intercept structure per assignment class
intercept.paths<-function( paths, links )
	lapply( paths,FUN=.intercept.paths,links=links )

# skim paths returns a demand matrix by performing a function on a set of numeric values
# corresponding to the links on each path between origin and destination
skim.paths<-function( paths, costs, empty.val=0.0, FUN="sum" ) {
	if ( FUN=="sum" )
		return( .skim.paths(paths,costs,empty.val) )
	else
		stop("Not implemented: Skims using functions other than 'sum'")
}

# Assignment Set
# This factory function builds a suitable object of one of the assignment set types
# The assignment sets themselves will need usable functions for computing diagnostic functions

# Structure of an assignment set:

# list with these elements:
#     network
#     cost.function (optional; default is to call a cost function for each class)
#     objective.function (optional; default is to do vector inner product of cost set and volume set)
#     ff.vol (free flow volume - zero flow in all classes)
#     ff.cost (cost function evaluated at zero flow in all classes)
#     classes (named list of assignment classes, each with these elements):
#           network.set -- routable subset of the network for assignment
#           demand (matrix)
#           cost.function (optional if assignment set cost function is defined)

cost.integrator <- function( cost.function, ff.vol, ff.cost, cong.vol, cong.cost, depth=0, tol=1e-4, max.depth=7, ... ) {
	# Integrate the cost function using adaptive quadrature with inside-interval tolerance testing
	# The tolerance indicates how close the mid-segment interpolated cost must be to its true value in order to avoid
	# recursing into each of the sub-intervals
	# May do as many as 2^depth+1 calls to cost.function 
	mid.cost  <- (cong.cost-ff.cost)/2
	vol.ivl   <- (cong.vol-ff.vol)/2
	mid.vol   <- ff.vol+vol.ivl
	true.cost <- cost.function(mid.vol)
	test.diff <- abs(mid.cost-true.cost)/true.cost
	if ( test.diff < tol || depth>=max.depth )  # limit on recursion
		result <- sum(vol.ivl*((ff.cost+true.cost)+(true.cost+cong.cost)))/2 # area for this part
	else {
		depth <- depth+1
		result<- Recall( cost.function, ff.cost, ff.vol, mid.vol, true.cost, tol=tol, depth=depth, ... ) +
	             Recall( cost.function, true.cost, mid.vol, cong.vol, cong.cost, tol=tol, depth=depth, ... )
	}
	attr(result,"max.depth")<-depth
	return(result)
}

# "correct" objective function
build.serious.objective.function<-function(cost.function, ff.vol, ff.cost, tol=1e-4, ...)
	function(cost, volume) cost.integrator(cost.function, ff.vol, ff.cost, volume, cost, tol, ...)

# expedient objective function
build.easy.objective.function<-function(cost.function, ff.vol, ff.cost, ...)
	function(cost,volume) sum( cost*volume )

# Construct an assignment.set with all the right pieces in all the right places
new.assignment.set <- function( network,classes,cost.volume.type=c("vector","matrix"),cost.function=NULL,
	make.objective.function=NULL, obj.tol=1e-4, ... ) {
	# We need cost functions to handle either a list/data.frame of volumes or a numeric vector
	# But we would like to figure out how to determine whether to boil down the volumes to a vector
	#  or pass the entire data.frame.
	# Suggestion: if any class provides a cost function, all of them receive a data.frame
	#   in that case, for classes with no cost function, the assignment set cost function should
	#   have a default process that takes a volume vector and returns a cost vector the unequipped class
	# If only the assignment set provides a cost function, then the function receives a vector

	# Check network
	if ( is.null(network) || class(network) != "highway.net" )
		stop("Assignment set requires a highway network")
	aset<-list(network=network)

	# Check cost.volume.type and construct a function to transform the volumes appropriately
	if ( length(cost.volume.type)>1 ) # Use default: "vector"
			cost.volume.type<-cost.volume.type[1]
	if ( ! (cost.volume.type%in%c("vector","matrix") ) )
		stop("Assignment set: Need vector or matrix cost function type.")
	if (cost.volume.type=="vector")
		cost.volume<-function(volume) rowSums(volume) # volume should be a matrix or data.frame
	else
		cost.volume<-function(volume) volume

	# Install cost function
	if ( is.null(cost.function) ) {
		# if there is no function for the assignment set, there must be one for each assignment class
		# cost functions for the assignment class get only a single parameter (volume)
		# cost functions for the assignment set are passed two parameters (volume,aset)
		if ( any(sapply( classes, function(x) { if (is.null(x$cost.function)) TRUE; FALSE } ) ) )
			stop("Assignment set is missing required cost.functions")
		# the master cost function will then apply the individual class functions
		aset$cost.function<-function(volume,aset) {
			as.data.frame(lapply(aset$classes,FUN=function(x,VOL) x$cost.function(cost.volume(VOL)),VOL=volume))
			}
	} else {
		# There is great flexibility in how the cost function works
		# It can call individual cost functions for only certain classes, build a partial matrix of
		# volumes with only certain classes, etc. (Set the cost.volume.type to "matrix" in that case)
		aset$cost.function<-function(volume,aset) {
			as.data.frame(cost.function(cost.volume(volume),aset))
			}
	}

	# Check classes
	if ( !is.list(classes) )
		stop("Assignment set cannot be built from non-list 'classes'")

	# Validate presence of required class elements
	bad.names <-names(classes)!=sapply(classes,function(x){ ifelse(is.null(x$name),"", x$name)})
	if ( any(bad.names) )
		stop("Assignment set classes do not have consistent names:\n",paste(names(classes)[bad.names],collapse=", "))
	missing.cost.function<-sapply( classes, function(x) { if (is.null(x$cost.function)) TRUE; FALSE } )
	if ( !all(missing.cost.function) && any(missing.cost.function) )
		message("Assignment set classes have incomplete class functions:",paste(names(classes)[missing.cost.function],collapse=", "))
	missing.demand<-sapply( classes, function(x) { if (is.null(x$demand)) TRUE; FALSE } )
	if ( any(missing.demand) )
		stop("Assignment set classes are missing required demand matrix:",paste(names(classes)[missing.demand],collapse=", "))
	missing.network.set<-sapply( classes, function(x) { if (is.null(x$network.set)||class(x$network.set)!="highway.network.set") TRUE; FALSE } )
	if ( any(missing.network.set) )
		stop("Assignment set classes are missing suitable network set:",paste(names(classes)[missing.demand],collapse=", "))
	aset$classes<-classes

	# Construct free-flow volume and cost, and build objective function for assignment
	aset$ff.vol<-free.flow(aset)
	aset$ff.cost<-aset$cost.function(aset$ff.vol)
	if ( is.null(make.objective.function) ) make.objective.function<-build.easy.objective.function
	aset$objective.function<-make.objective.function(cost.function,aset$ff.vol, aset$ff.cost, obj.tol,...)

	# Mark this as a known structure
	class(aset)<-"highway.assignment.set"

	return(aset)
}

# The following convenience functions make it easy to build a set of assignment classes, once
# you know the parameters for each class.
# Example:
#     class.list <- vector("list")
#     aclass <- make.assignment.class(network,"Class.1",demand.1,links.1,penalty.subset.1,cost.function.1)
#     class.list<-add.assignment.class(class.list,aclass) # preserves name, unlike c(...)
#     aclass <- make.assignment.class(network,"Class.2",demand.2,links.2,penalty.subset.2,cost.function.2)
#     class.list<-add.assignment.class(class.list,aclass)
#     ## etc...

make.assignment.class <- function( network, name, demand, link.subset=TRUE, penalty.subset=NULL, cost.function=NULL ) {
	aclass<-list( name=name,demand=demand )
	aclass$network.set<-.build.network.set(network,link.subset,penalty.subset)
	if (!is.null(cost.function)) aclass$cost.function<-cost.function
	return(aclass)
}

# Create a named list entry with the same name as the class
# Do not permit a new class to overwrite an existing class with the same name
# There are easy ways to remove the existing name first (e.g. class.list[[replace.name]]<-NULL)
add.assignment.class <- function( classes, aclass ) {
	if ( all(match(aclass$name,names(classes),nomatch=0)==0) ) {
		classes[[aclass$name]]<-aclass
	} else {
		warning("Cannot overwrite existing class",aclass$name)
	}
	return(classes)
}

# Cost functions
# Here is a helper to make a BPR-type function, using some static data
# This particular version wants a vector of volumes, which is suitable
# if we need a vector-based cost function
build.BPR.function <- function(cost.data) {
	# cost.data is either an environment, a list, or a data.frame
	# All-cap elements must be provided in cost.data
	# (either as scalars or as vectors that supply a value for each link in the network)
	return( with(cost.data, function(volume){TIME * ( 1 + (ALPHA/(CAPACITY^BETA)) * ( volume^BETA ) )} ) )
}

# Intercept Set, describes select link processing

# The intercept set is built at the level of the highway assignment
	# intercept is an intercept set that will assemble the intercept link volumes
	#    typically, this will be applied to all groups and we'll end up with a demand
	#    array with Z x Z x numSets dimensions, and a links matrix with L x numSets
	#    rows and columns attached to the intercept set

new.intercept.set <- function( links, filter.od=NULL ) {
	# links is a logical or numeric vector indexing the network links -- any path between
	# selected OD pairs that crosses one of these links is marked for inclusion by the
	# intercept.paths function.
	# filter.od takes a logical OD matrix and keeps only those OD pairs that are interesting
	#    (default is to keep all origins and destinations)
	if ( mode(links)!="logical" ) stop("links parameter for intercept.set must be a logical vector")
	if (is.null(filter.od)) filter.od<-function(od) return(od)
	iset <- list(links=links,filter.od=filter.od)
	attr(iset,"class")<-"intercept.set"
	return(iset)
}

