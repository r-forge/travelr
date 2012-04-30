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
#
#   In addition, the code for the two functions "cost.integrator" and "inner.integrator"
#   was adapted from Wikipedia.  These specific functions are copyrighted as noted above
#   and distributed under the Creative Commons Share-Alike License 3.0, which is available at
#   http://creativecommons.org/licenses/by-sa/3.0/
#   The "cost.integrator" and "inner.integrator" code is additionally available (as the
#   base code had not been modified at Wikipedia since before June, 2009) under the terms
#   of the GNU Free Documentation License, available here:
#   http://en.wikipedia.org/wiki/Wikipedia:GFDL
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
	mapply(function(ac,cost,penalties) .shortest.paths(ac$network,cost,penalties),
	       aset$classes,costs,MoreArgs=list(penalties=aset$penalties),
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
	result=mapply( function(ac,cost,penalties).build.and.load.paths(ac$network,cost,ac$demand,penalties),
				   aset$classes,costs,MoreArgs=list(penalties=aset$penalties),
				   USE.NAMES=TRUE, # Keep assignment class names on resulting volume set list
				   SIMPLIFY=FALSE) # and leave results as a list (each element of which is
				                   # a list with "paths" and "volumes" elements)
	paths = lapply(result,function(p)p$paths)      # Paths for all classes in one list
	volumes = as.data.frame(lapply(result,function(p)p$volumes))  # Volumes for all classes in one list
	return( list(paths=paths, volumes=volumes, result=result) )
}

# intercept.paths returns a list of OD matrices ("od") whose paths intercept selected links
# we get one intercept structure per assignment class
intercept.paths<-function( paths, links )
	lapply( paths,FUN=.intercept.paths,links=links )

# skim paths returns a demand matrix by performing a function on a set of numeric values
# corresponding to the links on each path between origin and destination
skim.paths<-function( paths, costs, empty.val=0.0, FUN="sum", ... ) {
	if ( FUN=="sum" ) {
		(mapply(function(p,c) .skim.paths(p,c,empty.val),
		    	paths, costs,
				USE.NAMES=TRUE, # Keep assignment class names on resulting volume set list
				SIMPLIFY=FALSE) # and leave the results as a list
		)
	} else
		stop("Skimming with functions other than 'sum' is not yet implemented")
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

# Construct an assignment.set with all the right pieces in all the right places
new.assignment.set <- function( network,classes,cost.volume.type=c("vector","matrix"),cost.function=NULL,
	objective.function=NULL, obj.tol=1e-4, ... ) {
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
	if ( is.null(network$Penalty.fields) || is.null(network$Penalty[[network$Penalty.fields[["Penalty.value"]]]]) )
		aset$penalties<-NULL
	else
		aset$penalties<-network$Penalty[[network$Penalty.fields[["Penalty.value"]]]] # may be NULL

	# Check classes
	if ( any(sapply(classes,function(c) class(c) )!="highway.assignment.class") )
		stop("Must use make.assignment.class and add.assignment.classes to build the 'classes' list")

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

	# Validate presence of required class elements
	if ( validate.assignment.classes(aset,classes) )
		aset$classes<-classes
	else
		stop("Invalid assignment class.")

	# Construct free-flow volume and cost, and build objective function for assignment
	aset$ff.vol<-free.flow(aset)
	aset$ff.cost<-aset$cost.function(aset$ff.vol,aset)
	if ( is.null(objective.function) )
		aset$objective.function<-build.general.objective.function( aset, aset$ff.vol, aset$ff.cost, tol=obj.tol)
	else
		aset$objective.function<-objective.function

	# Mark this as a known structure
	class(aset)<-"highway.assignment.set"

	return(aset)
}

# Convenience function for validating assignment classes
validate.assignment.classes <- function( aset, classes ) {
	bad.names <-names(classes)!=sapply(classes,function(x){ ifelse(is.null(x$name),"", x$name)})
	if ( any(bad.names) ) {
		warning("Assignment set classes do not have consistent names:\n",paste(names(classes)[bad.names],collapse=", "))
		return(FALSE)
	}
	# note that the following line works, even if aset doesn't have any classes yet (is.null(aset$classes))
	missing.cost.function<-sapply( c(aset$classes,classes), function(x) { if (is.null(x$cost.function)) TRUE; FALSE } )
	if ( !all(missing.cost.function) && any(missing.cost.function) ) {
		message("Assignment set classes have incomplete class functions:",
			paste(names(classes)[missing.cost.function],collapse=", ")) # TODO: clean up indexing to handle errors better
		return(FALSE)
	}
	missing.demand<-sapply( classes, function(x) { if (is.null(x$demand)) TRUE; FALSE } )
	if ( any(missing.demand) ) {
		warning("Assignment set classes are missing demand matrix:\n",
			paste(names(classes)[missing.demand],collapse=", "),"\nSupply zero demand matrix if demand is not yet known.")
		return(FALSE)
	}
	missing.network.set<-sapply( classes, function(x) { if (is.null(x$network.set)||class(x$network.set)!="highway.network.set") TRUE; FALSE } )
	if ( any(missing.network.set) ) {
		warning("Assignment set classes are missing suitable network set:",
			paste(names(classes)[missing.demand],collapse=", "))
		return(FALSE)
	}
	return(TRUE)
}

# The following convenience functions make it easy to build a set of assignment classes, once
# you know the parameters (network, demand, link and penalty subsets, and cost.function) for each class.
# Example:
#     class.list <- vector("list")
#     aclass <- make.assignment.class(network,"Class.1",demand.1,links.1,penalty.subset.1,cost.function.1)
#     class.list<-add.assignment.class(class.list,aclass) # preserves name, unlike c(...)
#     aclass <- make.assignment.class(network,"Class.2",demand.2,links.2,penalty.subset.2,cost.function.2)
#     class.list<-add.assignment.class(class.list,aclass)
#     ## etc...

make.assignment.class <- function( network, name, demand=NULL, link.subset=TRUE, penalty.subset=NULL, cost.function=NULL ) {
	if ( is.null(name) ) stop("Assignment class must have a valid name (Same rules as R identifier).")
	aclass<-list( name=name )
	aclass$network.set<-.build.network.set(network,link.subset,penalty.subset)

	if ( is.null(demand) ) {
		warning("No demand matrix supplied; zero matrix will be created")
		aclass$demand <- matrix(0,nrow=network$numZones,ncol=network$numZones)
	} else
		aclass$demand <- demand

	if (!is.null(cost.function)) aclass$cost.function<-cost.function
	class(aclass)<-"highway.assignment.class"
	return(aclass)
}

# Create a named list entry with the same name as the class
# Do not permit a new class to overwrite an existing class with the same name
# There are easy ways to remove the existing name first (e.g. class.list[[replace.name]]<-NULL)
add.assignment.class <- function( classes, aclass ) UseMethod("add.assignment.class",classes)

add.assignment.class.default <- function( classes, aclass )
	stop("Can only add assignment classes to a list of classes or to an assignment.set")

add.assignment.class.highway.assignment.set <- function( classes, aclass ) {
	aset <- classes
	if ( class(aclass)!="highway.assignment.class" )
		stop("Must use make.assignment.class and add.assignment.classes to build an assignment class")
	if ( validate.assignment.classes(aset,added.class ) ) {  # TODO: second parameter is wrong
		if ( all(match(aclass$name,names(aset$classes),nomatch=0)==0) ) {
			added.class <- list(aclass)
			names(added.class) <- aclass$name
			aset$classes<-c(aset$classes,added.class)
		} else {
			warning("Cannot overwrite existing class ",aclass$name)
		}
	}
	return(aset)
}

add.assignment.class.list <- function( classes, aclass ) {
	if ( all(match(aclass$name,names(classes),nomatch=0)==0) ) {
		classes[[aclass$name]]<-aclass
	} else {
		warning("Cannot overwrite existing class ",aclass$name)
	}
	return(classes)
}

# Function to update an entire assignment class... Can't imagine why, but here it is!
hwy.update.class <- function( aset, name, aclass ) {
	if ( is.null(name) || is.null(aclass) )
		stop("Cannot update assignment class unless name and new data are provided")
	if ( class(aclass) != "highway.assignment.class" )
		stop("Use make.assignment.class to construct the new class")
	if ( is.null(aset$classes[[name]]) )
		message("Adding assignment class named ",name)
	aset$classes[[name]] <- aclass
}

# Function to update the demand for an assignment class once it's in the assignment set
hwy.update.demand <- function( aset, name, demand ) {
	if ( is.null(name) )
		stop("Cannot update assignment class demand unless name is provided")
	if ( is.null(aset$classes[[name]]) )
		warning("No class named ",name)
	else if (!is.null(demand))
		aset$classes[[name]]$demand<-demand
	else
		warning("No new demand: assignment set was unchanged")
	return(aset)
}

# Function to update the penalty values for an assignment class
hwy.update.penalties <- function( aset, penalties ) {
	if ( is.null(aset$penalties) )
		stop("Number of penalties must be defined when network is built; only the values may change.")
	else if (!is.null(penalties) && length(penalties)==length(aset$penalties))
		aset$penalties<-penalties
	else
		stop(sprintf(gettext("Must provide %d penalties"),length(aset$penalties)))
	return(aset)
}

# Cost functions

# Here is a helper to make a BPR-type function, using some static data
# This particular version wants a vector of volumes, which is suitable
# if we need a vector-based cost function
build.BPR.cost.function <- function(cost.data) {
	# cost.data is either an environment, a list, or a data.frame
	# All-cap elements must be provided in cost.data
	# (either as scalars or as vectors that supply a value for each link in the network)
	FACTOR <- with(cost.data,ALPHA/(CAPACITY^BETA))
	return( with(cost.data, function(volume,...){TIME * ( 1 + FACTOR * ( volume^BETA ) )} ) )
}

# Objective Functions
build.BPR.objective.function <- function(cost.data) {
	# cost.data is either an environment, a list, or a data.frame
	# All-cap elements must be provided in cost.data
	# (either as scalars or as vectors that supply a value for each link in the network)
	BETA.OBJ<-with(cost.data,BETA+1)
	FACTOR <- with(cost.data,ALPHA/(CAPACITY^BETA.OBJ))
	with(cost.data, function(volume) { sum(volume * (TIME * ( 1 + FACTOR * ( volume^BETA ) ) ) ) } )
}	

# "correct" objective function when there is no easy analytic form
# Analytic form is to be preferred since it may be much faster
build.general.objective.function<-function(aset, ff.vol, ff.cost, tol=1e-4, max.depth=14 ) {
	cf <- function(volume) aset$cost.function(volume,aset)
	function(volume) cost.integrator(cf, ff.vol, ff.cost, volume, cf(volume), tol, max.depth)
}

# From Wikipedia:
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
