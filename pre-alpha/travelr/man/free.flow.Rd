\name{free.flow}
\alias{free.flow}

\title{
Free-flow Volumes
}
\description{
Build free-flow volumes from an assignment set
}
\usage{
free.flow(aset)
}
\arguments{
  \item{aset}{
	An \code{\link{assignment.set}}
}
}
\details{

\code{free.flow} produces a suitably structured set of numeric zero values for use in building costs;
the result is a set of link costs by assignment class at free flow conditions.  If the assignment set is
constructed using \code{\link{new.assignment.set}}, the free-flow volumes will be computed by this function
and already be stored in the \code{ff.vol} element of the \code{\link{assignment.set}}.
}
\value{
\code{free.flow} applies the assignment set cost function(s) to a vector or matrix of zeroes and returns the resulting link costs
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{assignment.set}}
}
