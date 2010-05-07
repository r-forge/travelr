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

\name{BPR function}
\alias{build.BPR.cost.function}
\alias{build.BPR.objective.function}
\alias{bpr.function}
\alias{bpr.functions}
\alias{BPR}

\title{
BPR cost and objective functions
}
\description{
Construct a BPR-style cost or objective function
}
\usage{
build.BPR.cost.function(cost.data)
build.BPR.objective.function(cost.data)
}
\arguments{
  \item{cost.data}{
	A data.frame, environment or list describing the BPR function parameters.
}
}
\details{

\code{build.BPR.cost.function} generates a function that takes a vector or matrix of volumes and applies parameters to
them to compute a congested travel time, using the venerable (or execrable, depending on your perspective) Bureau of
Public Roads formula (the Bureau of Public Roads was a precursor of the U.S. Federal Highway Administration).  The
resulting function can be used as a cost function for an assignment class within an \code{\link{assignment.set}}.

\code{build.BPR.objective.function} generates a function that can be used as an objective function in equilibrium
assignment algorithms that attempt to minimize generalized cost.  This evaluates analytically the integral of a BPR
cost function and returns a scalar value which is the sum of integrating from 0 to each of the link volumes.

A more general (but much slower) objective function can be built using \code{\link{build.general.objective.function}}.
Obviously, the user is also free to write an objective function on their own, consistent with whatever cost function
they may create.

\strong{Note:} This function does not actually calculate the BPR function.  Rather, it builds a new function that will
perform the BPR computation and that can be used as a \code{\link{cost.function}} in an \code{\link{assignment.set}}.
So you should call this function and then pass the resulting (unexecuted) function to the assignment set for use in
that context.

The functional form of the Bureau of Public Roads function looks like this:

\deqn{cost = TIME * \left(1+ALPHA*\left(\frac{volume}{CAPACITY}\right)^{BETA}\right)}{cost = TIME * ( 1 + ALPHA * (volume/CAPACITY)^BETA)}

The parameters in upper case must be supplied as elements of the \code{cost.data} parameter, or in the environment from
which build.BPR.function is called.  They must either have the same structure (either \dQuote{vector} or
\dQuote{data.frame}) as the set of volumes that will be passed to the function, or be amenable to conversion by the
standard R recycling rules.  The parameters that are supplied for the BPR function are defined as follows:

\describe{
\item{volume}{
The volumes for which costs are computed
}
\item{TIME}{
The free-flow link traversal time (returned when volume is zero)
}
\item{CAPACITY}{
The congested link capacity (often the \cite{Highway Capacity Manual (2000)} Level-of-Service D or E capacity)
}
\item{ALPHA}{
First BPR function parameter (classically, 0.15, but today often varying by link type)
}
\item{BETA}{
Second BPR function parameter (classically, 4.0, but today often varying by link type)
}
}

See \code{\link{assignment.set}} and \code{\link{cost.function}} for more information on how the volume structure type
is selected, and how these functions are used in highway assignment.
}
\value{
\code{build.BPR.cost.function} returns a function that takes a link vector (or matrix) of volumes and returns a link vector (or matrix) of link costs.
\code{build.BPR.objective.function} returns a function that takes a link vector (or matrix) of volumes and returns a scalar value indicating total cost.
}
\references{
Bureau of Public Roads, 1964. \emph{Traffic Assignment Manual}.  Urban Planning Division, U.S. Department of Commerce
Transportation Research Board, 2000. \emph{Highway Capacity Manual}
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{cost.function}} for the required interface of cost functions
	\code{\link{assignment.set}} for a description of how the cost function is used.
}
