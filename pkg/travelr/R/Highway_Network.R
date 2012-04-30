####################################################################################################
#   travelr\r\Highway_Network.R by Jeremy Raw  Copyright (C) 2010
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

# Highway network functions

# map.highway.nodes constructs a node map object for the network
#    map<-map.highway.net(links)
#    links$A.mapped<-map$map(links$A)
#    all(links$A==map$unmap(links$A.mapped))

# links should be a dataset or matrix with one row per network link
# nodes is a list of node identifiers (integer) used to
# identify ends of the links

# Link.fields identify the "From" and "To" node identifiers.  These should
# be scalar values suitable for use as a list index.

# Why NOT just use factors for the node identifiers, and make the "mapped" node fields into
# the integer factor code?

map.highway.nodes <- function( nodes, numZones ) {

	# Extract nodes in use from the network
	if ( any(nodes[1:numZones]!=c(1:numZones)) ) stop("Must have all zones (1 to",numZones,") present in node list")
		# Path building will be unhappy and issue a warning if there are destination zones
		#   that don't exist and thus can't be reached; plus you'll waste a lot memory and processing time
		#   building path trees that have nothing in them.
	node.map<-sort(unique(c(1:numZones,nodes[nodes>numZones])))
	base.index<-0

	# Construct functions with suitable closure for node mapping and unmapping
	map.env<-new.env(parent=parent.frame(2))	  # skip this function's environment
	assign("node.map",node.map,envir=map.env)     # but preserve necessary values
	assign("base.index",base.index,envir=map.env)
	map<-function(n) match(n,node.map,nomatch=base.index)
	unmap<-function(n) node.map[n]
	environment(map)<-map.env
	environment(unmap)<-map.env

	nmap<-list(map=map,unmap=unmap)
	attr(nmap,"numNodes")<-length(node.map)
	attr(nmap,"maxRawNode")<-max(node.map)
	attr(nmap,"class")<-"highway.node.map"
	return(nmap)
}
summary.highway.node.map<-function(object,...) return(unlist(attributes(object)[c("numNodes","maxRawNode")]))
print.highway.node.map<-function(x,...) print(summary(x))
length.highway.node.map<-function(x) return(attr(x,"maxRawNode"))

# Container for general network information
as.highway.net <- function( links, numNodes, numZones, Penalty=NULL, nodes=NULL, nodeMap=NULL, firstThruNode=NULL,
                         Link.fields=NULL, Penalty.fields=NULL ) {
	if ( is.null(links) )
		stop("Must supply data.frame with links")
	if ( is.null(numNodes) || numNodes<2 )
		stop(sprintf(gettext("Invalid number of Nodes: %d"),numNodes))
	if ( is.null(numZones) || numZones<1 || numZones>numNodes )
		stop(sprintf(gettext("Invalid number of Zones: %d"),numZones))
	if (any(grep("^\\.LinkID$",names(links)))) warning("Replacing existing .LinkID field in network links")
	links$.LinkID<-1:nrow(links)
	netwk<-list(Links=links,numNodes=as.integer(numNodes),numLinks=nrow(links))
	netwk$numZones<-as.integer(numZones)
	if (!is.null(nodes)) netwk$nodes<-nodes
	if (!is.null(nodeMap)) netwk$nodeMap<-nodeMap 
	if (!is.null(Link.fields) ) netwk$Link.fields<-Link.fields
	else netwk$Link.fields<-c(From="From",To="To")
	if (!is.null(Penalty)) {
		if (any(grep("^\\.PenaltyID$",names(Penalty)))) warning("Replacing existing .PenaltyID field in turn penalties")
		Penalty$.PenaltyID<-1:nrow(Penalty)
		netwk$Penalty<-Penalty
		if (!is.null(Penalty.fields)) netwk$Penalty.fields<-Penalty.fields
		else warning("If Penalty table is provided for highway network, you must specify Penalty.fields as well")
	} else {
		netwk$Penalty<-data.frame(Thru=as.integer(-1),From=as.integer(-1),To=as.integer(-1),.PenaltyID=as.integer(-1),Penalty.value=as.numeric(-1))
		netwk$Penalty.fields<-c(Thru="Thru",From="From",To="To",Penalty.value="Penalty.value")
	}
	if (is.null(firstThruNode)) netwk$firstThruNode<-as.integer(netwk$numZones+1)
	else netwk$firstThruNode<-as.integer(firstThruNode)
	class(netwk)<-"highway.net"
	return(netwk)
}

# Highway Network utility functions

summary.highway.net<-function(object,...) {
	sum.net<-c(numNodes=object["numNodes"],
	           numLinks=object["numLinks"],
			   numZones=object["numZones"],
			   firstThruNode=object["firstThruNode"]
			   )
	if (!is.null(object["Penalty"])) sum.net<-c(sum.net,numPenalty=nrow(object["Penalty"]))
	attr(sum.net,"class")<-"sum.highway.net"
	return(sum.net)
}
 
print.highway.net<-function(x,...) {
	print(summary(x))
}
 
print.sum.highway.net<-function(x,...) {
	cat("Highway Network:\n")
	cat("  Nodes:",x$numNodes,"Links:",x$numLinks,"Zones:",x$numZones,"\n")
	if (!is.null(x$Penalty)) cat("  Turn Penalties:",x$numPenalty,"\n")
	if (x$firstThruNode!=x$numZones) cat("  First Through Node:",x$firstThruNode,"\n")
}
 
# # TODO: add some functions to support igraph analysis
# # Turn the network into a first class igraph object:
# # Graph attributes:  numNodes, numLinks, numZones
# # Zone identification function as a graph attribute
# # Add a TAZ tag as an attribute for the vertices
# # Reconstruct the input links as edgelist attributes
# # Essentially all the graph information should be there
# 
# as.igraph <- function(n) {
# 	require(igraph)
# 	# TODO: verify that n has the right class
# 	g<-graph.edgelist(t(network$edges[1:2,]))
# 	g$numZones<-attr(n,"numZones")
# 	# TODO: renumber everything to 0-based, or rebuild the
# 	# appropriate elements like the offset list
# 	return(g)
# }
# 
# # TODO: construct a function to convert an appropriate igraph back into
# # a highway.net...

# # TODO: add some functions to support moving to/from an sp object
# # where nodes are points (or link end points), and links are lines

