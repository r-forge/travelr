####################################################################################################
#   travelr\r\Highway_Assignment.R by Jeremy Raw  Copyright (C) 2010
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

# This file defines the basic highway assignment mechanism and a series of algorithms, using the
# single/multi-class assignment sets to provide a set of abstract capabilities for evaluating
# the algorithms.

highway.assign <- function( aset, method=c("AON","MSA","Frank.Wolfe","ParTan"), control=vector("list",0) ) {
	# simple error-checking driver for highway assignment methods
	# aset: data for the assignment (network, demand, cost function)
	# method: Certain standard methods are defined in this module, and if the value of method (a character
	#   string) does not find a function with a name of "highway.assign.<<method>>", it will
	#   attempt to locate a built-in to perform the actual assignment (the built-ins are named
	#   ".highway.assign.<<method>>" and are hidden in the package namespace.
    # control: a list of control parameters:
	#   intercept=NULL     # an intercept.set object; intercept results will be returned, if supported by 'method'
    #   min.relative.gap=1e-4  # value below which algorithm has "converged"
    #   max.iter=100       # maximum number of assignment iterations
    #   max.time=600       # maximum algorithm runtime in seconds (for the impatient)
    #   opt.tol=.Machine$double.eps^0.5 # tolerance for internal line search
    #   verbose=0:5        # how much detail to report interactively during the run (bigger number ==> more wordy)
	#   log=FALSE          # if true, accumulates and returns a data.frame of iteration statistics
    # the implementation method should return a HighwayAssignment object (q.v.)

	# First check the aset parameter
	if (class(aset)!="highway.assignment.set")
		stop("Must provide an assignment.set object as first parameter (aset)")
	# Then find the appropriate method
	if (length(method)>1) {
		method<-method[1] # which means the default method is the first in the built-in list
		cat(sprintf(gettext("Using default assignment method '%s'\n"),as.character(method)))
	}
	method<-paste("highway.assignment.",as.character(method),sep="") # user can override by defining a suitable function
	a.func<-NULL
	if ( exists(method,mode="function") ) {
		a.func <- get(method,mode="function")
	} else {
		method<-paste(".",method,sep="")
		if ( exists(method,mode="function") ) a.func<-get(method,mode="function")
	}
	if ( is.null(a.func) )
		stop(paste(gettext("Could not find assignment method implementation"),sprintf("'%s'",method)))
	return(c(a.func(aset,control),list(method=method)))
}

# Parse control variable, including global defaults
# You'll get the value explicitly set, or the local default in the algorithm, or the global default
# from the control.defaults list
# Note that some algorithms like MSA may provide alternate "global" defaults

control.defaults <- list(
		intercept        = NULL,
		min.relative.gap = 1e-4,               # stop if relative.gap goes below this value
		max.iter         = 100,				 # for MSA, it's overridden to 4 and other stopping conditions are ignored
										     # set max.iter=0 for open-ended running
		max.elapsed      = 3600,				 # number of seconds - set to zero to run forever
		opt.tol          = .Machine$double.eps^0.5, # tolerance for internal line searches
		verbose          = 1,				 # depending on the algorithm, bigger numbers will produce more tracking output
		log              = FALSE				 # if true, return 'log' attribute (a data.frame) with assignment result details
	)

parse.control<-function( control, control.element, default.value=NULL ) {
#	cat("Parsing",control.element,"\n")
	element<-control[[control.element]]
	if ( !is.null(element) ) {
		result<-element
		default<-FALSE
	} else {
		default<-TRUE
		if ( !is.null(default.value) ) result<-default.value
		else                           result<-control.defaults[[control.element]]
	}
	if ( !is.null(result) ) attr(result,"default")<-default # can't put attribute on NULL
	return(result)
}

# Build a function to compute equilibrium statistics from user-supplied objective function
# 'flow' is a scalar representing the sum of all demands
# Computations based on Boyce, Ralevic-Dekic, and Bar-Gera, presentation at
# at the 16th Annual International EMME/2 Users’ Group Conference, Albuquerque, NM,
# March 18-20, 2002.
build.equil.stats.function <- function( objective.function, flow=0 ) {

	if ( isTRUE(all.equal(flow,0)) ) { avg.excess.cost.func <- function(gap)NULL }
	else                             { avg.excess.cost.func <- function(gap)gap/flow }

	start.time=proc.time()["elapsed"]
	run.time <- function() as.numeric(proc.time()["elapsed"]-start.time)

	# add additional values to the result list when calling this function
	# by providing named numeric scalar values in the ... parameter
	function( iter, cost, vol, diff, best.lower.bound=as.numeric(NA), extras=list() ) {
		# vol  is the vector of Equilibrium Path Volumes
		# diff is the vector of Shortest Path Volumes - Equilibrium Path Volumes
		# cost is the vector of link costs given current equilibrium path volumes
		iter             <-  iter
		elapsed          <-  run.time()
		objective.value  <-  objective.function(vol)
		gap              <-  -sum(cost*diff)
		lower.bound      <-  objective.value-gap
		best.lower.bound <-  max( best.lower.bound, lower.bound, na.rm=TRUE )
		relative.gap     <-  (objective.value-best.lower.bound)/abs(best.lower.bound)
		avg.excess.cost  <-  avg.excess.cost.func(gap)

		# The return form here produces a named vector of numeric values
		return ( c(list(iter=iter,elapsed=elapsed,
					    objective=objective.value,gap=gap,relative.gap=relative.gap,
					    lower.bound=lower.bound,best.lower.bound=best.lower.bound,
					    avg.excess.cost=avg.excess.cost),extras) )
	}
}

# To go with the equilibrium stats function, we need a function to test for convergence
# This function is extensible, and will use any control parameters that have prefixes
# of "min." or "max." with the rest of their name matching the equilibrium statistic names
build.convergence.test<-function( control, test=c("min.relative.gap","max.iter","max.elapsed") ) {
	# pre-compute a setup for convergence testing
	test.min.names <- test[grep("^min\\.",test)]
	test.max.names <- test[grep("^max\\.",test)]
	min.names<-gsub("^min\\.","",test.min.names)
	max.names<-gsub("^max\\.","",test.max.names)
	test.min <- numeric(0)
	for ( i in test.min.names ) test.min <- c(test.min,parse.control(control,i))
	test.max <- numeric(0)
	for ( i in test.max.names ) test.max <- c(test.max,parse.control(control,i))
# 	cat("Convergence Test::\n")
# 	cat("Min:",paste(test.min.names,sep=","),"Max:",paste(test.max.names,sep=","),"\n")
# 	cat( "Min:",paste(test.min,collapse=","),"Max:",paste(test.max,collapse=","), "\n")
	function(results) {
		any( unlist(results[min.names]) <= test.min ) ||
		any( unlist(results[max.names]) >= test.max )
	}
}

# Built-In Assignment Methods

# Not much error checking on built-in functions, since they are hidden in the namespace
#  and should only be called through the highway.assignment driver function, which
#  should do more checking

build.log.function <- function(control) {
	log <- parse.control(control,"log")
	if ( log ) {
		log.function<-function(new.results,log,verbose) {
			if ( is.data.frame(log) ) {
				nr <- as.data.frame(new.results)
				cat( paste(format(nr[1,]),collapse=", "), "\n" )
				return( rbind(log,nr) )
			} else {
				cat( paste(names(new.results),collapse=", "),"\n")
				return( as.data.frame(new.results) )
			}
		}
	} else
		log.function<-function(new.results,log,verbose) FALSE
	return(log.function)
}

# All-Or-Nothing
.highway.assignment.AON <- function(aset,control) {
	verbose <- parse.control(control,"verbose")
	if ( verbose>0 )
		message("All-or-Nothing (AON) highway assignment\n")
	assign.results<-build.and.load.paths(aset,aset$ff.cost)
	intercept<-(build.intercept.function(iset<-parse.control(control,"intercept"),aset))(assign.results$paths)
	return(list(aset=aset,costs=aset$ff.cost,
				paths=assign.results$paths,volumes=assign.results$volumes,
				iset=iset,intercept=intercept))
}

# Multiple Successive Averages
.highway.assignment.MSA <- function(aset,control) {
	log.function<-build.log.function(control)
	verbose <- parse.control(control,"verbose")
	if ( verbose>0 )
		message("Multiple Successive Averages (MSA) highway assignment\n")
	max.iter <- parse.control(control,"max.iter",4)
	if (attr(max.iter,"default")) {
		message(sprintf(gettext("Maximum iterations for MSA assignment set to default (%d)\n"),max.iter))
	} else if ( verbose>0 ) {
		message(sprintf("Maximum iterations for MSA assignment set to %d\n"),max.iter)
	}
	control$max.iter<-max.iter

	# Construct helper values and functions
	flow<-sum(sapply(aset$classes,function(ac)sum(ac$demand),USE.NAMES=FALSE))
	build.intercepts<-build.intercept.function(iset<-parse.control(control,"intercept"),aset)
	converged<-build.convergence.test(control,c("max.iter"))
	equil.stats<-build.equil.stats.function(aset$objective.function, flow)

	iter<-1
	cost <- aset$ff.cost
	load <- build.and.load.paths(aset,cost)
	vol  <- load$volumes
	cost <- aset$cost.function(vol,aset)
	intercept<-NULL
	best.lower.bound<-as.numeric(NA)
	while (TRUE) {
		load<-build.and.load.paths(aset,cost)
		vol.new<-load$volumes
		vol.diff <- vol.new-vol
# 		cat(iter,"Sum cost:",sum(aset$cost.function(vol,aset)),"\n")
# 		cat(iter,"Sum shortest-path cost:",sum(aset$cost.function(vol.new,aset)),"\n")
		new.results <- equil.stats(iter,cost,vol,vol.diff,best.lower.bound,extras=list(cost=cost))
		best.lower.bound<-new.results[["best.lower.bound"]]

		iter<-iter+1
		iter.weight <- 1/(iter+1)
		vol <- weighted.update.diff( iter.weight, vol, vol.diff )
		intercept<- weighted.update.intercept( iter.weight, intercept, build.intercepts(load$paths) )
		cost<-aset$cost.function(vol,aset)

		log <- log.function( new.results, log, verbose )
		if ( converged(new.results) )
			break
	}
	return(list(aset=aset,costs=cost,paths=load$paths,volumes=load$volumes,iset=iset,intercept=intercept,results=new.results,log=log))
}

# Frank.Wolfe Algorithm
.highway.assignment.Frank.Wolfe <- function(aset,control) {
	log.function<-build.log.function(control)
	verbose <- parse.control(control,"verbose")
	if ( verbose>0 )
		message("Frank-Wolfe highway assignment\n")

	# Construct helper values and functions
	flow<-sum(sapply(aset$classes,function(ac)sum(ac$demand),USE.NAMES=FALSE))
	build.intercepts<-build.intercept.function(iset<-parse.control(control,"intercept"),aset)
	converged<-build.convergence.test(control,c("min.relative.gap","max.iter","max.elapsed"))
	equil.stats<-build.equil.stats.function(aset$objective.function, flow)

	iter<-1
	cost <- aset$ff.cost
	load <- build.and.load.paths(aset,cost)
	vol  <- load$volumes
	intercept<-NULL
	best.lower.bound<-as.numeric(NA)

	lambda.func <- build.lambda.function(aset)
	lambda.search <- build.lambda.search( lambda.func, opt.tol=parse.control(control,"opt.tol") )

	while (TRUE) {
		cost <- aset$cost.function(vol,aset)
		load<-build.and.load.paths(aset,cost)
		vol.new<-load$volumes
		vol.diff <- vol.new-vol
		lambda <- lambda.search(vol,vol.diff)$lambda

		new.results <- equil.stats(iter,cost,vol,vol.diff,best.lower.bound,extras=list(lambda=lambda))
		best.lower.bound<-new.results[["best.lower.bound"]]

		vol <- weighted.update.diff( lambda, vol, vol.diff )
		intercept<- weighted.update.intercept( lambda, intercept, build.intercepts(load$paths) )

		log <- log.function( new.results, log, verbose )
		if ( converged(new.results) )
			break
		iter<-iter+1
	}
	return(list(aset=aset,costs=cost,paths=load$paths,volumes=load$volumes,iset=iset,intercept=intercept,results=new.results,log=log))
}

# ParTan (Parallel-Tangent) Algorithm
.highway.assignment.ParTan <- function(aset,control) {
	log.function<-build.log.function(control)
	verbose <- parse.control(control,"verbose")
	if ( verbose>0 )
		message("ParTan highway assignment\n")

	# Construct helper values and functions
	flow<-sum(sapply(aset$classes,function(ac)sum(ac$demand),USE.NAMES=FALSE))
	iset <- parse.control(control,"intercept")
	build.intercepts<-build.intercept.function(iset,aset)
	converged<-build.convergence.test(control,c("min.relative.gap","max.iter","max.elapsed"))
	equil.stats<-build.equil.stats.function(aset$objective.function, flow)

	iter<-1
	cost <- aset$ff.cost
	load <- build.and.load.paths(aset,cost)
	vol  <- load$volumes
	last.intercept<-intercept<-NULL
	best.lower.bound<-as.numeric(NA)

	lambda.func <- build.lambda.function(aset)
	lambda.search <- build.lambda.search( lambda.func, opt.tol=parse.control(control,"opt.tol") )

	while (TRUE) {
		cost <- aset$cost.function(vol,aset)
		load<-build.and.load.paths(aset,cost)
		intercept.new <- build.intercepts(load$paths)

		vol.new<-load$volumes
		vol.diff <- vol.new-vol
		lambda <- lambda.search(vol,vol.diff)$lambda
		vol.new <- weighted.update.diff( lambda, vol, vol.diff )
		intercept<- weighted.update.intercept( lambda, intercept, intercept.new )

		if ( exists("vol.last") ) {
			vol.diff.partan <- vol.new - vol.last
			alpha <- lambda.search(vol.last,vol.diff.partan,interval=c(0,5))
			vol.new <- weighted.update.diff( alpha, vol.last, vol.diff )
			intercept.new <- weighted.update.intercept( alpha, intercept.last, intercept.new )
		} else {
			alpha=0.0
		}
		
		new.results <- equil.stats(iter,cost,vol,vol.diff,best.lower.bound,extras=list(lambda=lambda,alpha=alpha))
		best.lower.bound<-new.results[["best.lower.bound"]]

		vol.last <- vol
		intercept.last <- intercept
		vol <- vol.new
		intercept <- intercept.new

		log <- log.function( new.results, log, verbose )
		if ( converged(new.results) )
			break
		iter<-iter+1
	}
	return(list(aset=aset,costs=cost,paths=load$paths,volumes=load$volumes,iset=iset,intercept=intercept,results=new.results,log=log))
}

# Pre-build the intercept function (to avoid if-test inside inner loop)
build.intercept.function <- function( iset, aset ) {
	if ( is.null(iset) ) f <- function(paths) NULL
	else {
		classes <- aset$classes   # list of matrices, one for each assignment class
		demand <- lapply(classes,function(x) x$demand)
		links <- iset$links     # vector of links of interest
		f <- function(paths) {
			od <- intercept.paths(paths,links)		# each OD here is just 0/1
			od <- mapply( function(od,d)d*od,       # Expand to list of OD matrices
						  od,demand,
						  USE.NAMES=TRUE, SIMPLIFY=FALSE )
			volumes <- mapply(.load.paths,
						  paths,od,
						  USE.NAMES=TRUE, SIMPLIFY=FALSE )
			return( list(od=od,volumes=volumes) )	# OD is list of matrices, per assignment class
													# volumes is list of volumes, per assignment class
		}
	}
	return( f )
}

# Helper functions for updating equilibrium iterations

# Weighted update functions combine iteration values

weighted.update.diff <- function( weight, base, new.diff ) base + weight * new.diff

weighted.update <- function( weight, base, new ) {
	if ( is.null(base) ) result <- new # ignoring weight in this case
	else result <- base*(1-weight) + weight * new
	return( result )
}

inner.weighted.intercept.function <- function(b,n,weight) {
	list( od      = weighted.update(weight,b$od,n$od),
		  volumes = weighted.update(weight,b$volumes,n$volumes) )
}

weighted.update.intercept<-function( weight, base, new ) {
	if ( is.null(base) || is.null(new) ) return(new)
	return( mapply(base,new,inner.weighted.intercept.function,vol=,SIMPLIFY=FALSE,weight=weight) )
}

# line search helper functions
# Do like this before starting any assignment iterations:
#      lambda.func<-make.lambda.func(cost.function,objective.function)
#      lambda.search<-make.lambda.search(lambda.func,opt.tol=1e-8)
# Then inside the iterations, when you need to find a lambda value, do this:
#      search.result<-lambda.search(volume,difference)
#      updated.volume <- volume + search.result$lambda * diff
#      updated.objective <- search.result$objective
# Look at the Frank-Wolfe or ParTan algorithms to see applications

build.lambda.function<-function(aset)
	function( lambda, vol, diff ) aset$objective.function(vol+lambda*diff)

build.lambda.search<-function(lambda.func,opt.tol=.Machine$double.eps^0.25) {
	function(vol,diff,interval=c(0,1)) {
		search.result<-optimize(lambda.func,vol=vol,diff=diff,interval=interval,tol=opt.tol)
		return(list(lambda=search.result$minimum,objective=search.result$objective))
	}
}
