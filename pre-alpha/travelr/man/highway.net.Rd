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
    Constructed by \code{\link{map.highway.nodes}}; used to consistently renumber nodes in the \code{links}, \code{nodes} and
    \code{Penalty} \code{data.frame}
}
  \item{firstThruNode}{
    Lowest node number through which paths can be built.  Usually either \code{1} or \code{numZones+1}
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
The highway network provides a standard format for storing network information.  The \code{highway.net} object
is simply a list that contains data and interpretive guidance that is used later when the network is converted
to other formats.  It provides a mechanism for mapping link and penalty fields to the values necessary for
use in highway assignment.  The field mapping is used when building a fast internal structure for highway
assignment.
}
\value{
Returns a regularized highway network structure as a list with class \code{highway.net}.
}
\author{
Jeremy Raw
}
\section{Under Development}{
The \code{highway.net} was assembled to facilitate conversion to the \pkg{igraph} package to support graph
analysis of the network (something the author finds useful), and subsequent developments will allow
\pkg{travelr} highway networks to interoperate with the spatial classes in \pkg{sp} to facilitate use of open
source GIS tools and analysis.  Doing either conversion by hand, if one has a table of node coordinates, is
straightforward.
}
\seealso{
    \code{\link{map.highway.nodes}}

	\code{\link{assignment.set}},\code{\link{highway.assign}}

	\code{\link{sample.networks}},\code{\link{tntp}}
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
