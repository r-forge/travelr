\name{Skim network}
\alias{skim network}
\alias{skim.paths}
\alias{gravity.model}

\title{
Skim a network
}
\description{
Apply a function to link values on each path
}
\usage{
skim.paths(paths,costs,empty.val=0.0,FUN=sum,...)
}
\arguments{  \item{paths}{
	A \code{path.set} build by \code{\link{build.paths}}
}
  \item{costs}{
	A vector or matrix of costs for the network
}
  \item{empty.val}{
	A value to store in skim matrix cells that are not crossed by any path (e.g. intrazonal cells)
}
  \item{FUN}{
	The function to apply to the \code{costs} vector associated with each of the \code{paths}; \code{FUN}
	should return a scalar value that can be stored in a numeric matrix
}
  \item{\dots}{
    Additional arguments to be passed to \code{FUN}
}
}
\details{

dQuote{Skimming a network} means computing a value for the shortest path between each origin and each destination in the
network.  \code{skim.paths} returns a square matrix where the number of rows and columns corresponds to the number of zones
in the network, and the value in each cell is the result of applying \code{FUN} to the \code{costs} values corresponding to
the links on each shortest path.

Most commonly, skimming is used to add up a cost value for all the links on the paths between all the origin/destination pairs.
It is also common to add up the values of other attributes (total distance, congested or free-flow time, miles of interstate
highway, etc.).

Sometimes, there are origin and destination pairs that do not have a path available.  Most commonly, these will be
intrazonal cells (the diagonal of the resulting matrix, where the origin and destination are the same).  Otherwise, they
will indicate that the origin and destination are not connected, which can be useful to know when debugging a highway
network.

\emph{The following description is not implemented yet:} However, by applying a different function, skimming can extract
a wide range of values, including the mean, minimum, or maximum link cost on a path, a true or false value depending on
whether a certain link exists in a path (which yields the same result as \code{\link{.intercept.paths}}, only more
slowly).  In general, if this function were designed to appeal to people who are primarily R users, rather than people
who are primarily travel modelers, it might also be called "path.apply" since the basic operation works very much like
\code{\link{tapply}} applied to a list of vectors of varying lengths.

}
\value{
\code{skim.paths} returns a numeric origin/destination matrix where each cell contains the
corresponding skim result; see details above.
}
\references{
Martin, W.A., and McGuckin, N.A., 1998, \emph{NCHRP Report 365: Travel Estimation Techniques for Urban Planning},
National Cooperative Highway Research Program / Transportation Research Board
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{build.paths}} and \code{\link{highway.assignment}} for ways to construct the paths parameter.
}
\examples{
\dontrun{
# A "three-step" model (no mode choice) with trip distribution via gravity model 
data(SiouxFalls)

# Trip Generation
productions<-rowSums(SiouxFalls.od)
attractions<-colSums(SiouxFalls.od)

# Highway Skims
cost.function<-with(SiouxFalls.net$links,function(...)FFTime)
aclass <- make.assignment.class(SiouxFalls.net,"All",SiouxFalls.od)
aset <- new.assignment.set(SiouxFalls.net,list(All=aclass),cost.volume.type="vector",cost.function=cost.function)
paths <- build.paths(aset,aset$ff.cost)
travel.times <- skim.paths(paths,aset$ff.cost)

# Trip Distribution (Gravity Model with gamma function)
base.distribution <- hwy.gamma.function(travel.times,-0.02,-0.123) # HBW coefficients from NCHRP 365
trip.table <- ipf(base.distribution,list(rows=productions, cols=attractions),method="absolute")
aset <- hwy.update.demand(aset,"All",trip.table)

# Trip Assignment
assignment.results <- highway.assign(aset,method="AON")
loaded.links <- assignment.results$volumes
}
}
