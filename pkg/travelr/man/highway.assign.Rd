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
%   and included in the R distribution (in directory 'share/licenses').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{highway.assign}
\alias{highway.assign}
\alias{highway.assignment}
\alias{highway assignment}
\title{
Perform highway assignment using a variety of methods
}
\description{
Perform highway assignment using a variety of methods such as All-or-Nothing (AON), Multiple Successive Averages (MSA),
or equilibrium approaches such as the Frank-Wolfe linearized convex solution strategy, and the ParTan variation.  Can be
extended with user-supplied algorithms.
}
\usage{
highway.assign(aset, method = c("AON", "MSA", "Frank.Wolfe", "ParTan" ), control=vector("list",0))
}
\arguments{
  \item{aset}{
  An \code{\link{assignment set}} whose classes will be assigned.
}
  \item{method}{
  A character string specifying the assignment method, which may be one of
  \dQuote{\acronym{AON}},  \dQuote{\acronym{MSA}}, \dQuote{Frank.Wolfe} or \dQuote{ParTan};
  if no method is specified, then \dQuote{\acronym{AON}} will be selected.  Alternatively,
  the method may select a user-defined assignment algorithm; see details.
}
  \item{control}{
  List of parameters for the assignment; varies by method, see details
}
}
\details{
The \code{highway.assign} function performs a traffic assignment using the network and demand data
specified in its first argument, which is an \code{\link{assignment.set}}.  The assignment method is
chosen using the \code{method} argument, which may refer to user-defined methods as described below.
The \code{control} argument is a list of values that set values for convergence criteria and adjust
the behavior of the algorithm in various ways.  Valid \code{control} elements for the built-in
algorithms are described below.

Most of the highway assignment algorithms reflect descriptions provided in \cite{Sheffi (1985)}.  ParTan
is described in \cite{Lee and Nie, 2001}.

\emph{Overview of Built-In Algorithms}

The \code{method} specifies the algorithm to be used for the traffic assignment.  Four common
assignment algorithms are built in:
\describe{
\item{\acronym{AON}}{\dQuote{All-or-Nothing} assignment.  The algorithm builds shortest paths fromthe free-flow speeds defined for the assignment set, and loads all the demand onto those paths
without considering capacity limits. The steps are these:
\enumerate{\item{Build paths}\item{Load Traffic}\item{Stop}}}
\item{\acronym{MSA}}{\dQuote{Multiple Successive Averages} does a simple form of capacity restraint
by averaging the volumes from successive AON assignments where the next assignment uses the costs from
the average volumes from all the previous assignment volumes.  The steps are these:
\enumerate{\item{Build paths using costs from average link volumes (initially free-flow)\item{Load
Traffic}\item{Compute the new average volume and the associated link costs}\item{If maximum number
of iterations is reached, stop, otherwise return to step 1.}}}}

\item{Frank.Wolfe}{Computes an equilibrium assignment using the simple, low-resource, but slowly
converging Frank-Wolfe algorithm.  This is a widely used method in travel demand modeling, but it
is gradually becomign obsolete due to advances in computing capacity and the desire to achieve faster
convergence to a narrower tolerance.  (The TravelR project exists in part to help explore practical
implementations of alternative algorithms.)  The steps are these:\enumerate{\item{Build paths using
costs from current equilibrium link volumes (initially free-flow)\item{Load Traffic}\item{Compute the
combination of equilibrium volume and shortest path volume that minimizes the objective function}
\item{Update the equilibrium volume to the combined value}\item{Recompute link costs from
equilibrium flow}\item{If convergence target is met, stop, otherwise return to step 1.}}}}

\item{ParTan}{The \dQuote{Parallel-Tangent} algorithm extends the Frank-Wolfe approach by rectifying
the search direction using a previous search direction.  This algorithm converges somewhat faster
than the basic Frank-Wolfe method. There is an additional line search for the second and subsequent
iterations that finds the lowest cost combination of the previous equilibrium volumes and the
current combined volume from a standard Frank-Wolfe step.  The steps are these:
\enumerate{\item{Build paths using costs from current equilibrium link volumes (initially free-flow)
\item{Load Traffic}\item{Compute the combination of equilibrium volume and shortest path volume that
minimizes the objective function} \item{Compute the combination of the new combined value and the
equilibrium result at the start of the previous iteration that minimizes the objective
function}\item{Save the current equilibrium volume for a later iteration}\item{Update the equlibrium
volume with the final combined volume}\item{Recompute link costs from the new equilibrium
flow}\item{If convergence target is met, stop, otherwise return to step 1.}}}}
}

Additional algorithms can be implemented in specially named functions provided by the user, through
a mechanism similar to (but less sophisticated than) \R's S3 methods.  The supplied method name is
prefixed by \dQuote{\code{highway.assignment.}} and a function by that name is sought in the calling
environment of the function.  The internal methods are named differently and are sought only when
the search for user-defined functions fails.  It is thus possible to supply an alternate
implementation for one of the internal methods by providing a function with a suitable name (e.g.
\code{highway.assignment.Frank.Wolfe})

The function that implements an assignment algorithm will be passed the \code{aset} and
\code{control} parameters, and should return a list of values, with elements comparable to those
returned by the standard algorithms.  An element called \code{method} will be concatenated to the
list returned from the algorithm function.

\emph{Control Parameters}

The \code{control} argument is a list of optional parameters that alter the behavior of the
assignment algorithms in various ways.  The sets of elements relevant to each built-in algorithm are
described first, and a description of each follows.

\describe{
\item{All Methods}{\code{intercept} (see below), \code{log}, \code{verbose}}
\item{\acronym{AON}}{No additional parameters}
\item{\acronym{MSA}}{\code{max.iter}}
\item{Frank.Wolfe}{\code{max.iter}, \code{min.relative.gap}, \code{max.elapsed}, \code{opt.tol}}
\item{ParTan}{\code{max.iter}, \code{min.relative.gap}, \code{max.elapsed}, \code{opt.tol}}
}

\tabular{rl}{
\code{intercept}\tab If this value is an \code{\link{intercept.set}}, then do select link processing (not set by default); see below \cr
\code{verbose}\tab If greater than 0, add additional messages (higher numbers imply more, but all
the functions are currently quite terse; default is \code{1}  \cr
\code{log}\tab If \code{TRUE}, return results of each iteration in a \code{data.frame}; defaults to \code{FALSE} \cr
\code{max.iter}\tab Maximum number of iterations before stopping (defaults to \code{4} for \acronym{MSA}, and
\code{100} for other algorithms \cr
\code{min.relative.gap}\tab Equilibrium algorithms will stop if \code{relative.gap} statistic drops
below this value (default \code{1e-4})  \cr
\code{max.elapsed}\tab Maximum allowable run-time in seconds (default \code{3600}) \cr
\code{opt.tol}\tab Tolerance for line search optimization (default \code{.Machine$double.eps^0.5})
}

\emph{Select Link Processing}

Travel modelers often seek information on which specific links are assigned demand for particular
zone pairs.  The select link results are reported either as a matrix of demand from origin to
destination that passes through one of the selected links, or as a vector of volumes tha results
when the selected demand matrix is assigned.

Internally, select link processing identifies links of interest, constructs a subset of the matrix
of the demand matrix that only includes origins and destinations, and assigns that demand to the
network.  Currently, select link processing applies to all assignment classes, and the selected
demand and link volumes for each class are maintained separately.  Naturally, one is rarely
interested in flows that follow a single path (say the shortest path).  Consequently, within the
assignment algorithms when an \code{intercept} is specified, the intercepts and demands are computed
at each iteration, and the results are combined using the same factors used for the total
equilibrium flow.  Thus, the intercepts will include a portion of traffic due to rarely used
alternate paths that may intercept a link of interest.

Needless to say, processing select link intercepts on a large network, or through many iterations, will be
substantially slower than simply computing a total equilibrium assignment.

See \code{\link{intercept.set}} for details about setting up a select link analysis.

}
\value{
Returns a list with various elements.  The possible elements are enumerated in the following table, which shows the
element name and description.  Certain elements are only
returned if suitable \code{control} values are provided, and they are so indicated.

\tabular{ll}{
aset\tab the assignment set which was assigned (identical to the \var{aset} argument \cr
costs\tab a \code{data.frame} with one column per assignment class and one row per link \cr
paths\tab the shortest path tree for the final assignment iteration resulting from the final costs \cr
volumes\tab a \code{data.frame} of volumes, with one column per assignment class and one row per link \cr
iset\tab the \code{control} argument describing the intercept (only present if it was supplied) \cr
intercept\tab A list of two elements: \code{od} a list of intercepted demand matrices, one for each class,
and \code{volumes} a data frame with one column per class of intercepted volumes (only present if an intercept
set was supplied in the \code{control} argument) \cr
results\tab \code{data.frame} of one row with the assignment statistics from the final iteration \cr
log\tab \code{data.frame} with assignment statistics from all iterations \cr
method\tab the \code{method} selected either as an argument or by default
}
}
\author{
Jeremy Raw
}
\references{
Lee, D.-H., and Nie, Y. \dQuote{Accelerating Strategies and Computational Studies of the Frank-Wolfe Algorithm for the
Traffic Assignment Problem}, 2001, \emph{Transportation Resarch Record}, 1771:97-105.
Sheffi, Y., 1985, \emph{Urban Transportation Networks}, Prentice-Hall; available online as a PDF
at \url{http://web.mit.edu/sheffi/www/urbanTransportation.html}
}
\seealso{
\code{\link{assignment.class}}, \code{\link{assignment.set}}, \code{\link{assignment.functions}} for
setting up highway assignments;

\code{\link{parse.control}} for interpreting assignment algorithm control values and defaults;

\code{\link{intercept.set}} and \code{\link{build.intercepts}} for details on setting up select link analysis

For a short complete example of a travel model: \code{example{\link{skim.paths}}}
}
