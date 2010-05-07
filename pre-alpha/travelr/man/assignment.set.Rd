%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Copyright (C) 2010 by Jeremy Raw
%   This program is free software; you can redistribute it and/or modify
%   it under the terms of the GNU General Public License as published by
%   the Free Software Foundation; either version 2 of the License, or
%   (at your option) any later version.
% 
%   This program is distributed in the hope that it will be useful,
%   but WITHOUT ANY WARRANTY; without even the implied warranty of
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%   GNU General Public License for more details.
% 
%   A copy of the GNU General Public License is available at
%   http://www.r-project.org/Licenses/
%   and included in the R distribution (in directory ‘share/licenses’).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{assignment.set}
\alias{assignment.set}
\alias{assignment set}
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
  An assignment class, usually created by \code{\link{make.assignment.class}}
}
  \item{\dots}{
  Additional parameters to be passed to the \code{cost.function}
}
}
\details{
The \code{assignment.set} structure is the essential parameter for any of the \code{\link{highway.assign}} algorithms.

The assignment set is constructed from a \code{\link{highway.net}}, and a list of \code{\link{assignment.class}}
elements (the \code{classes} parameter).  The network establishes the set of possible links, the set of possible
locations for turn penalties and prohibitions, and the initial turn penalty values.  The \code{classes} establish an
initial demand, a subset of links across which that class can be assigned, and a subset of penalties that are to
be applied for the class.

The \code{cost.function} parameter provides a cost function (see \code{\link{cost.functions}}, and
\code{\link{build.BPR.cost.function}} for further information).  If no \code{cost.function} is supplied,
each link is given a unit cost, so the shortest path in that case is always the one with the fewest links.  That is
only rarely useful.

A cost function providing a fixed link parameter can be provided, for example, by defining a simple function that
simply returns a link attribute:

\preformatted{cost.function <- with(network$Links,function(...) Distance}

where \code{Distance} is a column in the table of link attributes from which the network was constructed (and which
might, in this example, correspond to the link length, and thus yield paths representing the shortest distance between
each origin and destination).

The \code{objective.function} parameter provides an objective function for equilibrium assignment.  See
\code{\link{objective.functions}} for details.  A default function will be built (using the \code{cost.function} and the
\code{\link{cost.integrator}}) if an \code{objective.function} is not supplied.  If possible, writing an explicit
objective function will greatly improve the speed of the assignment algorithms.

The parameter \code{cost.volume.type} controls how volumes are passed to the cost and objective functions:
\dQuote{vector} produces a vector with the total volume on each link, while \dQuote{matrix} passes the entire cost
matrix (actually, a \code{data.frame} with one numeric column per assignment class, and one row per network link).
The \code{cost.function} should be written accordingly.

The \code{obj.tol} parameter controls the convergence tolerance passed to the \code{cost.integrator} function when
building a default objective function.  It may safely be ignored.

The resulting assignment set (which has a class of \code{highway.assignment.set}) has these elements:

\describe{
	\item{network}{The base highway network for all the assignment classes}
	\item{penalties}{The vector of initial turn penalties and turn prohibitions from the base network}
	\item{cost.function}{A constructed cost function that calls the user's cost function after converting the volumes to the \code{cost.volume.type}}
	\item{classes}{The assignment classes for this set}
	\item{ff.vol}{Free-flow volume: a set of zeroes with one column per assignment class, and one row per network link; see \code{\link{free.flow}}}
	\item{ff.cost}{The set of routing costs that result from applying \code{cost.function} to \code{ff.vol}}
	\item{objective.function}{The supplied or constructed equilibrium objective function}
}

\code{hwy.update.class} can replace (or add) an entire assignment class to an assignment set.  The assignment class
should have been built by \code{\link{make.assignment.class}}

\code{hwy.update.demand} returns the assignment set after the named class has had its demand matrix updated to the \code{demand} parameter value.

\code{hwy.update.penalties} returns the assignment set with updated penalty values; this function is useful for
dynamically updating penalty values during an assignment algorithm.

\code{add.assignment.class}, when applied to a \code{assignment.set} will add an additional assignment class to the
set of assignment classes already defined.  It the named class already exists, its values will be updated.

The various \code{hwy.update} functions can be replaced with suitable recursive list indexing to find and then add or
replace the target value, and the user may wish to do that if speed is of utmost concern.  Otherwise, these functions
provide an additional level of error-checking that may be useful when first developing a model or algorithm.

}

\value{
\code{new.assignment.set} returns an \code{assignment.set} structure describing demand and network characteristics for each class of user in highway assignment.

\code{hwy.update.class}, \code{hwy.update.demand} and \code{hwy.update.penalties} return the updated assignment set

\code{add.assignment.class} returns the updated assignment set
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{highway.assign}}

	\code{\link{cost.functions}} for a discussion of cost functions, and \code{\link{build.BPR.cost.function}} for a helper function.

	\code{\link{objective.functions}} for a discussion of objective functions, \code{\link{build.general.objective.function}}, and \code{\link{build.BPR.objective.function}}
}
