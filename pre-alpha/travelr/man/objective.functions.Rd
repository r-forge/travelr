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

\name{objective.functions}
\alias{objective.function}
\alias{objective.functions}
\alias{build.general.objective.function}
\alias{cost.integrator}

\title{
Construct objective functions
}
\description{
An objective function provides a measure of least cost used in evaluating equilibrium conditions during highway
assignment.  This help section describes strategies for building objective functions.
}
\usage{
build.general.objective.function(aset, ff.vol, ff.cost, tol = 1e-04, max.depth = 14)
cost.integrator(cost.function,ff.vol,ff.cost,cong.vol,cong.cost,tol=1e-8,max.depth=14)
}
\arguments{
\item{aset}{
	An \code{\link{assignment.set}} with a \code{\link{cost.function}}
}
\item{ff.vol}{
	Free flow (or lower bound) volumes (vector or matrix depending on type of cost function)
}
\item{ff.cost}{
	Link costs corresponding to free-flow (or lower bound) volumes
}
\item{tol}{
	Convergence tolerance for cost function integration using adaptive quadrature
}
\item{max.depth}{
	Maximum allowed recursion level during cost integration (a maximum of \code{2^max.depth+1} calls to the cost function will be made)
}
\item{cost.function}{
	The cost function (volume/delay) that relates link volume to link cost
}
\item{cong.vol}{
	Congested (or upper bound) volumes
}
\item{cong.cost}{
	Link costs corresponding to congested (or upper bound) volumes)
}
}
\details{
Objective functions are called from within the assignment algorithms (\code{\link{highway.assign}} like this:

\preformatted{objective.function(volume)}

Parameter:
\describe{
\item{volume}{volumes for which the objective function is to be computed; see below}
}

The objective function returns a numeric scalar value that will be minimized by the highway assignment function.  Note
that it is possible to write objective functions that ignore certain assignment classes (for example, if loading a
certain class always at free-flow conditions).  It is nevertheless usually more efficient in those cases to construct
multiple assignment sets and load the classes separately.

The helper function \code{build.general.objective.function} will construct a usable \code{objective.function}
from an arbitrary \code{cost.function}.  The free-flow volumes and costs are ordinarily computed by
\code{\link{new.assignment.set}} and are available as the \code{ff.flow} and \code{ff.cost} elements, respectively.

The resulting objective function uses \code{cost.integrator} to perform numerical integration.  The cost function is
integrated from free-flow to congested volumes.  While the resulting objective function is reasonably accurate and will
work with almost any cost function, it will always be faster (and possibly much more accurate) to build an analytic
objective function (see the helper function \code{\link{build.BPR.objective.function}} for an example of how to do that
for the common \acronym{BPR} functional form.

The \code{cost.integrator} function is also available as a general-purpose \R utility which will evaluate integrals for
functions taking a vector of parameters.  In the case of a vector value, the function to be integrated is presumed to be
the summation of the cost.function evaluated for each of the vector values.  This is considerably more flexible than the
standard \code{integrate} function, but also a good deal slower.  The \code{cost.integrator} function uses adaptive
quadrature and Simpson's rule for approximating definite integrals.

Helper functions are also available to construct cost (and objective) functions in the classic form known as
\acronym{\link{BPR}} (Bureau of Public Roads, a precursor of the Federal Highway Administration).

}
\value{
The \code{objective.function} required by \code{\link{highway.assign}} returns a numeric vector or data.frame of the same shape as \code{volume}

\code{build.general.objective.function} returns a function that can be used as an objective function in equilibrium highway assighment

\code{cost.integrator} returns a (scalar) numeric value that results from integrating the supplied function between lower and upper bounds

}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{assignment.set}} for a description of how the objective function is supplied.

	\code{\link{bpr.functions}} for BPR helper functions.

	\code{\link{highway.assign}} for how objective functions are used.
}
