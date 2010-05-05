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

\name{Assignment Functions}
\alias{assignment.functions}
\alias{build.convergence.test}
\alias{build.equil.stats.function}
\alias{build.lambda.function}
\alias{build.lambda.search}
\alias{weighted.update}
\alias{weighted.update.diff}
\alias{weighted.update.intercept}

\title{
Support functions for developing highway assignment algorithms
}
\description{
These functions are used to construct functions and perform operations in support of highway assignment.
Generally, they are only of interest to developers implementing new assignment algorithms.
}
\usage{
build.convergence.test(control,test=c("min.relative.gap","max.iter","max.elapsed"))
build.equil.stats.function(objective.function,flow=0)
build.lambda.function(aset)
build.lambda.search(lambda.func,opt.tol=.Machine$double.eps^0.25)

weighted.update(weight,base,new)
weighted.update.diff(weight,base,new.diff)
weighted.update.intercept(weight,base,new)
}
\arguments{
\item{control}{
	Assignment algorithm control parameters (a named list)
}
\item{test}{
	Which control parameters indicate convergence criteria (parameter name must start with
	\code{.min} or \code{.max} indicating how to test; the remainder of the name must match
	a value computed in the equilibrium statistics function; see details)
}
\item{objective.function}{
	The objective function to be minimized during the equilibrium assignment process
}
\item{flow}{
	The sum of all the demand matrices in the assignment set (used to compute average excess cost)
}
\item{lambda.func}{
	The function for which a zero is found to compute the optimum step size in a search direction
}
\item{opt.tol}{
	Convergence tolerance for the root of lambda.func
}
\item{aset}{
	An \code{\link{assignment.set}} with a \code{\link{cost.function}}
}
\item{tol}{
	Convergence tolerance for cost function integration using adaptive quadrature
}
\item{weight}{
	Weight of new values in computing weighted average of base and new
}
\item{base}{
	Base values to be updated
}
\item{new.diff}{
	Difference between base and new values (weight applied only to difference)
}
\item{new}{
	New values to use for updating (weight applied to new values, 1-weight applied to base)
}
}
\details{
These functions will generally only be of interest to the developers of new highway assignment algorithms.

\code{parse.control} extracts a parameter from the \code{control} list typically provided to the
\code{\link{highway.assign}} function.  Certain parameters have global defaults, which may be overridden
by a local default if the \code{default} parameter is supplied.  The global defaults are as follows:
\tabular{llll}{
min.relative.gap\tab numeric\tab \code{1e-4} \tab Target for relative gap statistic \cr
max.iter\tab numeric\tab \code{100} \tab Maximum iteration, except 4 for method=dQuote{\acronym{MSA}}) \cr
max.elapsed\tab numeric \tab \code{3600} \tab seconds since the algorithm started \cr
opt.tol\tab numeric \tab \code{.Machine$double.eps^0.5} \tab tolerance for internal line searches) \cr
verbose\tab numeric \tab \code{1} \tab Debugging level (higher numbers may generate more output; 0 for silent) \cr
log\tab logical \tab \code{FALSE} \tab If true, save a \code{data.frame} with the equilibrium statistics from each
iteration
}
Any parameter for which no default is provided will return \code{NULL} when requested.

Arbitrary additional parameters may be defined.  The \code{control} parameter is just a named list
of values, and the function simply looks up the name and returns the corresponding element.  It
differs from simple list indexing in that it can provide a default value.

Certain of these functions build other functions, tying certain fixed parameters into the function's
environment in order to reduce the burden of redundant interpretation and computation during highway
assignment.

Perhaps the most obscure are the following:

\code{build.equil.stats.function} builds a function that computes performance statistics for an iteration of
the equilibrium assignment algorithm.  The built function is defined as follows:
\preformatted{
	function(iter, cost, vol, diff, best.lower.bound=as.numeric(NA), extras=list())
}
\describe{
	\item{iter}{The current iteration, numeric}
	\item{cost}{The current cost structure (usually a \code{data.frame}; see \code{\link{cost.functions}})}
	\item{vol}{The current volume structure (usually a \code{data.frame}; see \code{\link{load.paths}}) }
	\item{diff}{A \code{data.frame} containing the difference between the most recent \acronym{AON} volume and the
	current equilibrium volume}
	\item{best.lower.bound}{The \code{best.lower.bound} as of the previous iteration, or \code{as.numeric(NA)} if
	the first iteration}
	\item{extras}{a named list of additional (scalar) values to be placed in the output list}
}
The standard equilibrium statistics function returns a list of computed statistics that can be compared against
target values to assess the convergence of an assignment algorithm.  The output includes:
\describe{
   \item{iter}{Current iteration, supplied as an input parameter}
   \item{elapsed}{Time, in seconds, elapsed since the function was defined}
   \item{objective}{Value of the objective function at the start of this iteration}
   \item{gap}{Value of the \dQuote{gap} statistic}
   \item{relative.gap}{Value of the \dQuote{relative gap} statistic}
   \item{lower.bound}{Value of the \dQuote{lower bound} statistic}
   \item{best.lower.bound}{The greatest of either the \code{best.lower.bound} parameter or the newly computed \code{lower.bound}}
   \item{avg.excess.cost}{Value of the Average Excess Cost (normalized gap) statistic}
   \item{extras}{Any values passed in through the \code{extras} parameter}
}

The \code{build.convergence.test} function returns a function that will compare the computed equilibrim statistics to target
values supplied either through the \code{control} parameter for the \code{\link{highway.assign}} function, or as a default value
through \code{parse.control}, or as a global default value.

The general mechanism for constructing a test is to provide names in the \code{test} vector that match the names of
values in the \code{control} list.  Those names should begin either with \dQuote{\code{min.}} or \dQuote{\code{max.}}
indicating how the equilibrium statistic will be compared.  The name without the prefix should correspond to the name of
a statistic in the result list from the equilibrium statistics function (including, possibly, a name in the
\code{extras} list.

The \code{highway.assign} source code provides examples of how to apply these functions.
}

\value{
\code{parse.control} returns a value for the named highway assignment control parameter

\code{build.convergence.test} returns a function that does a convergence test

\code{build.equil.stats.function} returns a function that builds a list of assignment statistics for reporting and convergence testing

\code{build.lambda.function} returns a function with a zero at a point of interest

\code{build.lambda.search} returns a function that finds the zero of a lambda function given upper and lower limits

\code{weighted.update} returns a value the same shape as its first argument that is the weighted average of its arguments

\code{weighted.update.diff} returns a value the same shape as its first argument that is the first argument plus weight times its second argument

\code{weighted.update.intercept} returns an \code{\link{intercept list}} (with two elements, \code{od} and \code{volumes}) that is the weighted average of the two intercept lists passed to it
}
\author{
Jeremy Raw
}
\seealso{
\code{\link{highway.assign}}, \code{\link{cost.functions}}
}
