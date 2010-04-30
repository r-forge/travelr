\name{assignment.set}
\alias{assignment.set}
\alias{new.assignment.set}
\alias{cost.volume.type}
\alias{hwy.update.class}
\alias{hwy.update.demand}
\alias{hwy.update.penalties}
\alias{add.assignment.class.highway.assignment.set}

\title{
Assignment Sets: Managing multi-class highway assignment
}
\description{
Keep network variants and demand sets for use in multi-class highway assignment.
}
\usage{
new.assignment.set(network,classes,cost.volume.type=c("vector","matrix"),cost.function=NULL,objective.function=NULL,obj.tol=1e-4,...)
hwy.update.demand(aset,name,demand)
hwy.update.penalties(aset,penalties)
hwy.update.class(aset,name,aclass)
\method{add.assignment.class}{highway.assignment.set}(classes,aclass)
}
\arguments{
  \item{network}{
	The base highway network on which this assignment set will be loaded.
}
  \item{classes}{
	A list of assignment classes, or for the generic \code{hwy.update.class}, either a list of assignment classes or an assignment set
}
  \item{cost.function}{
	A function generating costs from volumes for the assignment set (optional if each assignment class has its own); see details
}
  \item{cost.volume.type}{
	Indicates how the loaded volume should be passed to the cost function
}
  \item{objective.function}{
	An objective function for equilibrium assignment; see details
}
  \item{obj.tol}{
    Required precision for objective.function (to support, for example, adaptive quadrature for integrating a cost.function)
}
  \item{aset}{
  An assignment set
}
  \item{name}{
	A name (character string) for the assignment class.
}
  \item{demand}{
    A demand matrix for the named assignment class
}
  \item{penalties}{
  A numeric vector of revised penalty values
}
  \item{aclass}{
  An assignment class
}
  \item{\dots}{
  Additional parameters to be passed to the \code{cost.function}
}
}
\details{

The \code{assignment.set} structure is the essential parameter for any of the \code{\link{highway.assignment}} algorithms.

The parameter \code{cost.volume.type} controls how volumes are passed to the cost and objective functions:
\dQuote{vector} produces a vector with the total volume on each link, while \dQuote{matrix} passes the entire cost
matrix

\code{make.objective.function} returns a function that is passed the cost vector(matrix) and volume vector(matrix) (the
latter either a vector or matrix per \code{cost.volume.type}) and returns a scalar value which is minimized by the
assignment.  If you want to specify your own objective function in detail (e.g. by analytically integrating the cost
function), just make a function that returns the function you wrote and place that function here.

\code{hwy.update.demand} returns the assignment set with updated demand matrix for the named class.
}
\value{
\code{new.assignment.set} returns a structure describing demand and network characteristics for each class of user in highway assignment.
\code{hwy.update.demand} and \code{hwy.update.penalties} return the updated assignment set
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{highway.assign}}
}
