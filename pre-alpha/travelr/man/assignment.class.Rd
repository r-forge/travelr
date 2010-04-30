\name{assignment.class}
\alias{assignment.class}
\alias{make.assignment.class}
\alias{add.assignment.class}
\alias{add.assignment.class.list}

\title{
Managing individual highway assignment classes
}
\description{
Keep network variants and demand sets for use in multi-class highway assignment.
}
\usage{
make.assignment.class(network, name, demand, link.subset=TRUE, penalty.subset=NULL, cost.function=NULL)
add.assignment.class(classes,aclass)
\method{add.assignment.class}{list}(classes,aclass)
}
\arguments{
  \item{network}{
	The base highway network on which this assignment set will be loaded.
}
  \item{name}{
	A name (character string) for the assignment class.
}
  \item{demand}{
    A demand matrix for the named assignment class
}
  \item{link.subset}{
	An index vector that can be used to select links in the base network that will be available for this assignment
	class.
}
  \item{penalty.subset}{
	An index vector that selects which penalties (from the network's turn penalty table) will be applied to this
	assignment set.
}
  \item{cost.function}{
	A function generating costs from volumes for the assignment set (optional if each assignment class has its own); see details
}
  \item{classes}{
	A list, either empty or containing previously added assignment classes
}
  \item{aclass}{
  A highway.assignment.class constructed by \code{make.assignment.class}
}
}
\details{
Use \code{make.assignment.class} to construct a new assignment class.  Use \code{add.assignment.class} to add another
assignment class to a list of assignment classes.  A list of assignment classes is required for the \code{classes} parameter
in \code{\link{new.assignment.set}}.

It is common to have a non-null \code{link.subset}, with the most common situation being to distinguish networks in which
all links are available from one that only contains links available to a certain vehicle class (e.g. trucks or high-occupancy
vehicles).  It is similarly useful to have a non-null \code{penalty.subset}, either for penalties that are only
applicable during certain periods of the day, or that are only applicable to certain vehicle classes.

The \code{cost.function} parameter should be supplied if the ultimate assignment set is expecting the cost function to
vary by class (for example by applying a PCE correction to truck volumes when computing routing cost for passenger
vehicles, but not applying the PCE to the routing cost for trucks themselves), or when certain classes are assigned to
free-flow paths regardless of the level of congestion (effectively pre-loading the network with those vehicles).  Note,
however, that getting fancy with cost functions is not advised since it can wreak havoc with the convergence of
equilibrium assignment algorithms, and the simplest possible cost function model should be sought.  If a single cost
function applies to all assignment classes, it is simpler and recommended to use a single cost function for the entire
assignment set (and to supply it through \code{\link{new.assignment.set}}.

\code{add.assignment.class} is generic, and can add an assignment class to an \code{\link{assignment.set}} or to an
existing list of assignment classes previously created with \code{add.assignment.class}.  It performs a variety of
tests to ensure that the proposed assignment class has everythign it needs to operate successfully. Be very atttentive
to how the \code{\link{cost.function}} is going to be applied by the assignment set.
}
\value{
\code{make.assignment.class} returns a list of elements with R class \code{highway.assignment.class} suitable for
building a \code{\link{assignment.set}}
\code{add.assignment.class} returns its first argument, to which \code{aclass} has been added.
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{assignment.set}},\code{\link{highway.assign}},
	\code{\link{add.assignment.class.highway.assignment.set}}
}
