\name{assignment.set}
\alias{assignment.set}
\alias{new.assignment.set}
\alias{make.assignment.class}
\alias{add.assignment.class}
\alias{get.assignment.classes}

\title{
Manage sets of highway assignment classes
}
\description{
Keep network variants and demand sets for use in multi-class highway
assignment.
}
\usage{
new.assignment.set(network,classes,cost.function=NULL,cost.volume.type=c("vector","matrix"),make.objective.function=NULL,obj.tol=1e-4,...)
make.assignment.class(network, name, demand, link.subset=TRUE, penalty.subset=NULL, cost.function=NULL)
add.assignment.class(classes,aclass)
}
\arguments{
  \item{network}{
	The base highway network on which this assignment set will be loaded.
}
  \item{classes}{
	A list of elements, each of which specifies an assignment class; See details.
}
  \item{cost.function}{
	A function generating costs from volumes for the assignment set (optional if each assignment class has its own); see details
}
  \item{cost.volume.type}{
	Indicates how the loaded volume should be passed to the cost function: \dQuote{vector} produces a vector with the total volume on each link, while \dQuote{matrix} passes the entire cost matrix
}
  \item{make.objective.function}{
	A function to generate the objective function for equilibrium assignment; the function is passed cost and volume (the latter either a vector or matrix per \code{cost.volume.type}) and
returns a scalar value which is minimized by the assignment
}
  \item{obj.tol}{
    Required precision for objective.function (to support, for example, adaptive quadrature for integrating a cost.function)
}
  \item{name}{
	A name (character string) for the assignment class.
}
  \item{demand}{
    A demand matrix for this assignment class
}
  \item{link.subset}{
	An index vector that can be used to select links in the base network that will be available for this assignment
	class.
}
  \item{penalty.subset}{
	An index vector that selects which penalties (from the network's turn penalty table) will be applied to this
	assignment set.
}
  \item{aclass}{
  A list of elements that will form one element of a \code{class.data} structure.  See details.
}
  \item{...}{
  Additional parameters to be passed to the \code{cost.function}
}
}
\details{
The \code{assignment.set} structure is the essential parameter for any of the \code{\link{highway.assignment}} algorithms.
}
\value{
Returns a structure describing demand and network characteristics for each class of user in highway assignment.
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{highway.assign}}
}
\examples{
\dontrun{print("Examples yet to come")}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
