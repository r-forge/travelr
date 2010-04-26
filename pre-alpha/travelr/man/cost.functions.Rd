\name{cost.functions}
\alias{cost.functions}
\alias{build.BPR.function}

\title{
Construct cost functions
}
\description{
Construct cost functions for volume/delay computations in networks
}
\usage{
build.BPR.function(cost.data)
}
\arguments{
  \item{cost.data}{
	A data.frame, environment or list describing the BPR function parameters.  See details.
}
}
\details{
\code{build.BPR.function} generates a function that takes volumes and applies parameters to them to compute a congested
travel time.  The functional form is the Bureau of Public Roads function.  This function is quite flexible, and can use
fixed coefficients or link-specific values for any of the coefficients.  It will also work to generate multi-class costs
provided each class has the same functional form and the costs in each class do not depend on the other classes.  The
coefficients can vary by class, however.
}
\value{
\code{build.BPR.function} returns a function that takes a link vector (or matrix) of volumes and returns a link vector (or matrix) of link costs.
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{assignment.set}} for a description of how the cost function is used.
}
\examples{
\dontrun{print("Examples yet to come")}
}
