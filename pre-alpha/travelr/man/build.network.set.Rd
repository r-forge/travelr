\name{build.network.set}
\alias{build.network.set}
\alias{.build.network.set}
\title{
Construct internal structures for shortest path evaluation
}
\description{
Build suitable internal structures for computing shortest paths on a highway network.
}
\usage{
.build.network.set(network, link.subset, pen.subset=NULL)
}
\arguments{
  \item{network}{
	A highway network
}
  \item{link.subset}{
	A vector index indicating which network links are active for this class
}
  \item{pen.subset}{
	A vector index indicating which penalty values are active for this class
}
}
\details{
This function is usually only called from within \code{\link{new.assignment.set}}.
The \code{link.subset} and \code{pen.subset} indexes will be those specified for
each assignment class, with a default automatically supplied when the assignment
class is created that will select \emph{all} links but \emph{no} penalties. 
}
\value{
Returns a version of the network suitable for use in building shortest paths efficiently.
}
\author{
Jeremy Raw
}

\seealso{
	\code{\link{shortest.paths}}
}
\examples{
\dontrun{print("Examples yet to come.")}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
