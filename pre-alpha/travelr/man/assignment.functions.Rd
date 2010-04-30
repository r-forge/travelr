\name{Assignment Functions}
\alias{build.convergence.test}
\alias{build.general.objective.function}
\alias{build.equil.stats.function}
\alias{build.lambda.function}
\alias{build.lambda.search}
\alias{cost.integrator}
\alias{parse.control}
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
parse.control(control,control.element,default.value=NULL)

build.convergence.test(control,test=c("min.relative.gap","max.iter","max.elapsed"))
build.equil.stats.function(objective.function,flow=0)
build.lambda.function(aset)
build.lambda.search(lambda.func,opt.tol=.Machine$double.eps^0.25)

build.general.objective.function(aset, ff.vol, ff.cost, tol = 1e-04, max.depth = 14)
cost.integrator(cost.function,ff.vol,ff.cost,cong.vol,cong.cost,tol=1e-8,max.depth=14)

weighted.update(weight,base,new)
weighted.update.diff(weight,base,new.diff)
weighted.update.intercept(weight,base,new)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{control}{
	Assignment algorithm control parameters
}
\item{control.element}{
	Parameter to retrieve
}
\item{default.value}{
	Default value to supply if control parameter is not set (if NULL, global defaults apply)
}
\item{test}{
	Which control parameters indicate convergence criteria (parameter name must start with
	\code{.min} or \code{.max} indicating how to test; the remainder of the name must match
	a value computed in the equilibrium stats function)
}
\item{objective.function}{
	The objective function to be minimized during the equilibrium assignment process
}
\item{flow}{
	The sum of all the demand matrices in the assignment set (used to compute average excess cost)
}
\item{cost.function}{
	The cost function (volume/delay) that relates link volume to delay penalty
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
\item{ff.vol}{
	Free flow (or lower bound) volumes (vector or matrix depending on type of cost function)
}
\item{ff.cost}{
	Link costs corresponding to free-flow (or lower bound) volumes
}
\item{tol}{
	Convergence tolerance for cost function integration using adaptive quadrature
}
\item{cong.vol}{
	Congested (or upper bound) volumes
}
\item{cong.cost}{
	Link costs corresponding to congested (or upper bound) volumes)
}
\item{max.depth}{
	Maximum allowed recursion level during cost integration (a maximum of \code{2^max.depth+1} calls to the cost function will be made)
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
Details on assignment functions are yet to come.
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
