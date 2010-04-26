####################################################################################################
#   travelr\r\Path_Functions.R by Jeremy Raw  Copyright (C) 2010
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

# R interface to low-level functions for shortest paths

# network.set structure for path building
# This is "C level" so all implied node/link/turn indices are converted to 0-base for internal use

.zero.base<-function(v) v-1
.one.base <-function(v) v+1

.build.network.set <- function( network, link.subset, pen.subset=NULL ) {
	# Construct network structures for use in path building

	# Note that for network subsets (e.g. a network without HOV lanes), the cost and volume vectors will still have
	# entries for all the network links, but the link.subset and pen.subset can be just a selected set, and only
	# the selected set will be inserted into the edge list and offsets for path-building.
	# The correspondence to the base tables of links and penalties is through .LinkID and .PenaltyID

	links<-network$Links[link.subset,]
	Link.fields<-network$Link.fields
	edges<-matrix( c(A=.zero.base(links[[Link.fields["From"]]]),
					 B=.zero.base(links[[Link.fields["To"]]]),
					 Link=.zero.base(links$.LinkID)),
				   ncol=3, nrow=nrow(links) )
	dimnames(edges)<-list(NULL,c("A","B","Link"))

# 	cat("edges:\n")
# 	print(edges)

	Penalty.fields<-network$Penalty.fields
	turns<-NULL
	if ( !is.null(pen.subset) ) {
		penalties<-network$Penalty[pen.subset,]
		if ( !is.null(penalties) ) 
			turns<-matrix( c(Node=.zero.base(penalties[[Penalty.fields["Thru"]]]),
							 Parent=.zero.base(penalties[[Penalty.fields["From"]]]),
							 Child=.zero.base(penalties[[Penalty.fields["To"]]]),
							 Turn=.zero.base(penalties$.PenaltyID)),
						   ncol=4,nrow=nrow(penalties) )
			penalty.values<-network$Penalty[[network$Penalty.fields["Penalty.value"]]]
	}
	if (is.null(turns)) { # no turn penalty subset supplied
		turns<-matrix( -1, ncol=4, nrow=1 ) # dummy penalty table for merge
		penalty.values<-NULL
	}
	dimnames(turns)<-list(NULL,c("Node","Parent","Child","Turn"))
# 	cat("turns:\n")
# 	print(turns)

	offsets<-merge(aggregate(data.frame(Start=edges[,"Link"]),by=list(NODE=edges[,"A"]),FUN=min),
			       aggregate(data.frame(End=edges[,"Link"]),by=list(NODE=edges[,"A"]),FUN=function(x){max(x)+1}))
	offsets<-merge(data.frame(NODE=0:.zero.base(network$numNodes)),offsets,by="NODE",all.x=TRUE)
	turnoff<-merge(aggregate(data.frame(TurnOn=turns[,"Turn"]),by=list(NODE=turns[,"Node"]),FUN=min),
			       aggregate(data.frame(TurnOff=turns[,"Turn"]),by=list(NODE=turns[,"Node"]),FUN=function(x){max(x)+1}))
	offsets<-merge(offsets,turnoff,by="NODE",all.x=TRUE)
	offsets[is.na(offsets)]<-0
# 	cat("offsets:\n")
# 	print(offsets)

	# transpose matrices for efficient processing during path-building
	offsets<-t(data.matrix(offsets))
	storage.mode(offsets)<-"integer"
	edges  <-t(edges)
	storage.mode(edges)<-"integer"
	turns  <-t(turns)
	storage.mode(turns)<-"integer"

	# construct return value
	net.set<-list( edges=edges, turns=turns, offsets=offsets, penalty.values=penalty.values )
	attr(net.set$edges,"numNodes")<-network$numNodes
	attr(net.set$edges,"numZones")<-network$numZones
	attr(net.set$edges,"numLinks")<-network$numLinks
	attr(net.set$edges,"firstThruNode")<-as.integer(.zero.base(network$firstThruNode)) # Important: 0-based at C level
	attr(net.set,"class")<-"highway.network.set"

	return( net.set )
}

.shortest.paths <- function(network.set,costs) {
	paths<- .Call("shortest_paths",
				  network.set$edges,network.set$offsets,network.set$turns,
				  costs,network.set$penalty.values) # penalty.values may be NULL
	return(paths)
}

.build.and.load.paths <- function(network.set,costs,demand) {
	loaded.paths <- .Call("build_and_load_paths",
				  network.set$edges,network.set$offsets,network.set$turns,
				  costs,network.set$penalty.values,demand) # penalty.values may be NULL
	return(list(paths=loaded.paths$paths,volumes=loaded.paths$volumes))
}

.load.paths <- function(paths,demand) {
	volumes <- .Call("load_paths",paths,demand)
	return(volumes)
}

.skim.paths <- function(paths,costs,empty.value=0.0) {
	demands <- .Call("skim_paths",paths,costs,empty.value)
	return(demands)
}

.intercept.paths <- function(paths,links) {
	if ( length(links)!=attr(paths,"numLinks") )
		stop("links vector for .intercept.paths (",length(links),
		     ") must match number of links in network (",attr(paths,"numLinks"),")")
	intercepts <- .Call("intercept_paths",paths,links)
	return(intercepts)  # at this point, an OD matrix whose paths intercept links
}

# .walk.paths returns a list of vectors of .LinkID values
# Note that link IDs are zero based in the C code, and one-based in R
# so a suitable conversion is performed going in and coming out
# if permute is TRUE, evaluate all combinations of o and d
# otherwise match them pairwise with recycling
.walk.paths <- function(paths,origins,dests,permute=TRUE) {
	if ( !permute && length(origins)!=length(dests) )
		stop("Origins and Dests must be same length if permute is FALSE")
	if ( length(origins)==0 || length(dests)==0 )
		return(vector("list",0))
	if (permute) c.func<-"walk_paths"
	else         c.func<-"walk_pairs"
	origins<-.zero.base(origins) ; storage.mode(origins)<-"integer"
	dests<-.zero.base(dests)     ; storage.mode(dests)  <-"integer"
	path.list <- .Call(c.func,paths,origins,dests)
	return( lapply(path.list,FUN=function(p) .one.base(p)) )
}
