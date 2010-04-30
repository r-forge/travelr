\name{highway.assign}
\alias{highway.assign}
\alias{highway.assignment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Perform highway assignment using a variety of methods
}
\description{
Perform highway assignment using a variety of methods such as
All-or-Nothing (AON), Multiple Successive Averages (MSA), or
equilibrium approaches such as the Frank-Wolfe linearized convex
solution strategy.
}
\usage{
highway.assign(aset, method = c("AON", "MSA", "Frank.Wolfe", "ParTan" ), control=vector("list",0))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{aset}{
%%     ~~Describe \code{aset} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{control}{
%%     ~~Describe \code{control} here~~
}
}
\details{
Details on highway assignment are yet to come.
}
\value{
Returns a highway.assignment.result object
}
\author{
Jeremy Raw
}
\seealso{
\code{\link{assignment.set}}, ~~~
}
\examples{
\dontrun{print("Examples yet to come")}
}
