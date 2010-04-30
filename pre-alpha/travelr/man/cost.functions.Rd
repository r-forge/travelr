\name{cost.functions}
\alias{cost.function}
\alias{cost.functions}

\title{
Construct cost functions
}
\description{
Cost functions (Volume/Delay) for highway assignment
}
\details{
Cost functions are called in two forms:

\preformatted{cost.function(volume,aset)    # Assignment set}
\preformatted{cost.function(volume)         # Assignment class}

In the first case, the volumes and the assignment set itself are passed to the function (so that different
assignment classes in the set may have their costs evaluated separately).

In the second case, the function is applied in the context of an assignment class, so only the volumes are passed.

If the assignment set is defined with a \code{\link{cost.volume.type}} of dQuote{vector}, the \code{volume} passed to
both functions will be a vector corresponding to the sum of all the class volumes.  Otherwise, if the
\code{\link{cost.volume.type}} is dQuote{matrix}, a data frame will be passed with the volume for each class in its
own column.

A cost function must be written for an \code{\link{assignment.set}}, in order to provide the basis for path-building
in equilibrium assignment, although a workable default will be built if cost functions are supplied for each of the
assignment classes.  These functions can be very simple (for example, by setting the cost to a fixed vector
such as a free-flow link traversal time), or extremely complex (when assessing the interacting costs of different
vehicle classes).
}
\value{
\code{cost.function} returns a numeric vector or data.frame of the same shape as \code{volume}
}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{assignment.set}} for a description of how the cost function is used.
}
