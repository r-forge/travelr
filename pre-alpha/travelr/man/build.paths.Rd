\name{build.paths}
\alias{build.paths}
\alias{load.paths}
\alias{build.and.load.paths}
\alias{free.flow}
\alias{skim.paths}
\alias{intercept.paths}

\title{
Construct shortest paths and perform assignment operations on them.
}
\description{
Build shortest paths from an assignment set, load demands on the paths
}
\usage{
build.paths(aset,costs)
build.and.load.paths(aset,costs)
load.paths(aset,paths)
skim.paths(paths,costs,empty.val=0.0,FUN=sum)
free.flow(aset)
}
\arguments{
  \item{aset}{
	An \code{\link{assignment.set}}
}
  \item{costs}{
	A vector or matrix of costs for the network.  Unused links may be zero or NA.
}
  \item{paths}{
	A \code{path.set} build by \code{\link{build.paths}}
}
  \item{empty.val}{
	A value to store in skim matrix cells that are not crossed by any path (e.g. intrazonal cells)
}
  \item{FUN}{
	The function to apply to the \code{costs} vector associated with each of the \code{paths}; \code{FUN}
	should return a scalar value that can be stored in a numeric matrix
}
}
\details{

These functions are used to support link-based highway assignment algorithms, as well as operations on path sets
resulting from highway assignment.  \code{build.paths} makes a path set given a set of costs. The first application of
\code{build.paths} is usually performed using zero volumes, corresponding to a free-flow condition.

With the resulting paths, the \code{load.paths} function will place the demand matrix onto the network links according to
the shortest paths.  This corresponds to an all-or-nothing assignment.  It differs from the \dQuote{AON} highway assignment method
in that it does not diagnostic checking and has a simpler set of parameters.

\code{build.and.load.paths} performs a build.paths operation followed directly by the equivalent of load.paths.

The costs for path building are a link vector or a list or data.frame with one column per assignment class, and
one row for each link in the network.  See \code{\link{assignment.set}} for details.  Ordinarily the costs are
constructed by passing the link volumes to the cost.function stored within the assignment set.

\code{free.flow} produces a suitably structured set of numeric zero values for use in building costs;
the result is a set of link costs by assignment class at free flow conditions.  If the assignment set is
constructed using \code{\link{new.assignment.set}}, the free-flow volumes will already be stored in the
\code{ff.vol} element of the \code{\link{assignment.set}}.
}
\value{
\code{build.paths} constructs a path set, using a vector of costs that should have been built by the
\code{cost.function} associated with the \code{assignment.set}.
\code{load.paths} constructs a vector or matrix of volumes (depending on the type of \code{\link{assignment.set}}) by placing the demand matrix onto the shortest paths. 
\code{build.and.load.paths} returns a list consisting of the loaded volumes (the same as \code{load.paths}) and the
corresponding path set as would have been returned from \code{build.paths}
\code{free.flow} applies the assignment set cost function(s) to a vector or matrix of zeroes and returns the resulting link costs
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{highway.assign}} for general equilibrium assignment methods for highway networks.
}
\examples{
\dontrun{print("Examples yet to come")}
}
