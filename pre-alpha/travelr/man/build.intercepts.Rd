\name{build.intercepts}
\alias{build.intercepts}
\alias{build.intercept.function}
\title{
Perform 'Select Link' Analysis for Highway Network
}
\description{

Define intercept function for select link analysis

}
\usage{
build.intercept.function(iset,aset)
}
\arguments{
  \item{iset}{
  Returned from the \code{\link{intercept.set}} function, a list defining links and origin/destination pairs of interest
}
  \item{aset}{
  An \code{\link{assignment.set}} describing the demand and other characteristics used to build the path set
}
}
\details{
Details yet to come.
}
\value{
Returns a list with two components: \code{od} containing the selected origin/destination matrix, and
\code{links} which is the result of loading the selected demand onto the shortest paths.
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{intercept.set}}, \code{\link{build.paths}}

}
\examples{
\dontrun{print("Examples yet to come.")}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
