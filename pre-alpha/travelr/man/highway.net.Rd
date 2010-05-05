%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright (C) 2010 by Jeremy Raw
%   This program is free software; you can redistribute it and/or modify
%   it under the terms of the GNU General Public License as published by
%   the Free Software Foundation; either version 2 of the License, or
%   (at your option) any later version.
% 
%   This program is distributed in the hope that it will be useful,
%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%   GNU General Public License for more details.
% 
%   A copy of the GNU General Public License is available at
%   http://www.r-project.org/Licenses/
%   and included in the R distribution (in directory ‘share/licenses’).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{highway.net}
\alias{highway.net}
\alias{as.highway.net}
\alias{summary.highway.net}
\alias{print.highway.net}

\title{
Highway network management functions
}
\description{
Functions for constructing a highway network suitable for use in shortest path analysis and highway assignment functions.
}
\usage{
as.highway.net(links, numNodes, numZones,
    Penalty = NULL, nodes = NULL, nodeMap = NULL,
    firstThruNode = NULL, Link.fields = NULL, Penalty.fields = NULL)
\method{summary}{highway.net}(object,...)
\method{print}{highway.net}(x,...)

}
\arguments{
  \item{links}{
    A \code{data.frame} of links in the network; see details
}
  \item{numNodes}{
    The number of nodes in the network.  The nodes must account for all 'From' and 'To' nodes in the network links
}
  \item{numZones}{
    The number of zones (origins and destinations) in the network.  The zones must have sequential numbers from \code{1}
    to \code{numZones}
}
  \item{Penalty}{
    An optional \code{data.frame} describing network turn (junction) penalties; see details
}
  \item{nodes}{
    An optional node specification, typically a \code{data.frame}, that can contain useful information such as geographical coordinates
}
  \item{nodeMap}{
	Optional element for mapping node numbers; constructed by \code{\link{map.highway.nodes}}; used to consistently
	renumber nodes in the \code{links}, \code{nodes} and \code{Penalty} \code{data.frame}
}
  \item{firstThruNode}{
    Lowest node number through which paths can be built.  Usually either \code{1} or (the default) \code{numZones+1}
}
  \item{Link.fields}{
    A named character vector describing which columns contain required fields in the links \code{data.frame}; see
    details
}
  \item{Penalty.fields}{
    A named character vector describing which columns contain required fields in the Penalty \code{data.frame} 
}
  \item{object}{
    The \code{highway.net} object to be summarized
}
  \item{x}{
    The \code{highway.net} object to be printed
}
  \item{\dots}{
    Additional arguments passed to generic methods
}
}
\details{
The highway network provides a standard format for storing network information.  The \code{highway.net} object is simply
a list that contains data and interpretive guidance that is used later when the network is converted to other formats
(particularly to a \code{\link{network.set}} for use in highway assignment).  It provides a mechanism for mapping link
and penalty fields to the values necessary for use in highway assignment.  The field mapping is used when building a
fast internal structure for highway assignment.

The most important elements for highway routing are the \code{links}, which must include at a minimum fields \code{From}
and \code{To} indicating node numbers that are connected by the link.  At present, the highway network is represented as
a directional network with explicit coding.  That is, if a two-way connection is present between two nodes, then two
links should be coded into the network.  Many GIS systems, however, make it much easier to code a network link as a
single feature with two sets of attributes (one for each direction).  In that case, either the \R function
\code{\link{reshape}}, or a simple combination of subsetting and \code{rbind} may prove useful to pre-format the
network into the necessary format.

The number of rows in the \code{links} table defines the number of links in the network

If the network does not already contain suitably formatted fields, the optional \code{Link.fields} character vector may
be used to identify alternate field names.  If \code{Link.fields} is provided, the element names must include
\code{From} and \code{To}, and the values should be the actual field names include the \code{Links} table.

Also important are the \code{numNodes} and \code{numZones} parameters, which indicate how many node numbers are present
in the network (a value that is required in order to construct a shortest path tree), and \code{numZones} indicating the
number of terminal nodes in the network, which defines how many paths will be built.  These term \dQuote{Zones} is short
for \dfn{Traffic Analysis Zone} or \dfn{Transportation Analysis Zone}, which are also often referred to by acronym as
\acronym{TAZ}.  In the world of travel demand models, paths through the network are typically only required to span the
zones, and the \pkg{travelr} path builder algorithm will stop once it has built paths from each origin zone to every
destination zone (so some non-terminal nodes on the network may not have paths built for them from every origin).

Another common convention in transportation networks is to forbid building paths through terminal nodes (zones).  The
\code{firstThruNode} parameter controls that behavior.  Any node numbered below \code{firstThruNode} is treated as a
terminal node and no paths will be built through such nodes.  In general application, \code{firstThruNode} will be set
to \code{1} if paths are allowed to be built through zones, and otherwise to \code{(numZones + 1)} in case the zones are
true terminal nodes.

To facilitate the creation of compact data structures, \pkg{travelr} generally requires highway networks to use a
compact sequence of nodes.  The first \code{numZones} nodes (defining an integer sequence \code{1:numZones}) establish
the size of demand matrices (which must have \code{numZones} rows (origins) and columns (destinations)).  Nodes in the
integer sequence from \code{numZones + 1} to \code{numNodes} are intermediate nodes through which paths may be built.
Since many existing travel model networks have discontinuous node numbering sequences (for example, if certain node
sequences are associated with particular jurisdictions or facility types), the \code{nodeMap} element can contain a
\code{\link{map.highway.nodes}} object to facilitate renumbering the nodes into a suitable sequence.

The \code{Penalty} table, if it is provided, is a table of junction movements to which penalty or prohibition values can
be attached.  A penalty is a positive numeric value, whereas a prohibition is any negative value.  Penalty values may be
updated during assignment only for the movements identified in this table.  Each table row is assigned a unique
identifier in field \code{.PenaltyID}, and the vector of penalty or prohibition factors is stored in ascending order by
\code{.PenaltyID}.

The \code{Penalty} table itself has three required fields identifying the nodes through which the penalized
movements pass, conventionally called \code{From}, \code{Thru} and \code{To}, but these can be mapped to other field
names by using the \code{Penalty.fields} character vector.  If \code{Penalty.fields} is provided, the element names must
include \code{From}, \code{Thru} and \code{To}, and the values should be the actual field names include the \code{Penalty}
table.  Penalties that include nodes that are not actually in the network itself, or not in a subset, are allowed and are
simply ignored.

The \code{nodes} element is not used directly, but provides a documented place to store node values that might be useful
for junction modeling, or for coordinates that could be used to present the network spatially.
}
\value{
Returns a regularized highway network structure as a list with class \code{highway.net}.  The elements of this list
include the following (some of which optionally may not be present, as noted):
}
\author{
Jeremy Raw
}
\section{Under Development}{
The \code{highway.net} was designed to facilitate conversion to the \pkg{igraph} package in order to perform graph
analysis of the network (something the author finds useful), and subsequent developments will allow \pkg{travelr}
highway networks to interoperate with the spatial classes in \pkg{sp} to facilitate use of open source GIS tools and
analysis.  Doing either conversion by hand, if one has a table of node coordinates, is straightforward.
}
\seealso{
	\code{\link{sample.networks}},\code{\link{tntp}} to get networks to work with

    \code{\link{map.highway.nodes}} for a tool to renumber network nodes

	\code{\link{assignment.set}},\code{\link{highway.assign}} to use a highway network
}
\examples{
\dontrun{
links <- read.table("My_Network.net",header=TRUE)
link.names <- c(From="A",To="B")
numZones <- 10
firstThruNode <- 11
allNodes <- unique(c(links[link.names$A],links[link.names$B]))
numNodes <- length(allNodes)
if ( numNodes != max(allNodes) )
    stop("You should use map.highway.nodes to renumber the nodes")
my.network <-
    as.nighway.net(links,numNodes,numZones,
        firstThruNode=firstThruNode, Link.fields=link.names)

}
}
