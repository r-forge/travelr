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

\name{parse.control}
\alias{parse.control}

\title{
Parse control parameter for highway assignment
}
\description{
Extract control parameters for highway assignment, supplying defaults when available.
}
\usage{
parse.control(control,control.element,default.value=NULL)

}
\arguments{
\item{control}{
	Assignment algorithm control parameters (a named list)
}
\item{control.element}{
	Parameter to retrieve, a character string
}
\item{default.value}{
	Default value to supply if control parameter is not set (if NULL, global defaults apply if
	availble; see details)
}
}
\details{
This function will generally only be of interest to the developers of new highway assignment algorithms.

\code{parse.control} extracts a parameter from the \code{control} list typically provided to the
\code{\link{highway.assign}} function.  Certain parameters have global defaults, which may be overridden
by a local default if the \code{default} parameter is supplied.  The global defaults are as follows:
\tabular{llll}{
\code{min.relative.gap}\tab \code{1e-4} \tab Target for relative gap statistic \cr
\code{max.iter}\tab \code{100} \tab Maximum iteration, except 4 for method=\dQuote{\acronym{MSA}}) \cr
\code{max.elapsed} \tab \code{3600} \tab Maximum time to run after the algorithm starts \cr
\code{opt.tol} \tab \code{.Machine$double.eps^0.5} \tab Tolerance for internal line searches. \cr
\code{verbose} \tab \code{1} \tab Debugging level (higher numbers may generate more output; 0 for silent) \cr
\code{log} \tab \code{FALSE} \tab If true, save a \code{data.frame} with the equilibrium statistics from each
iteration
}
Any parameter for which no default is provided will return \code{NULL} when requested.

Arbitrary additional parameters may be defined.  The \code{control} parameter is just a named list
of values, and the function simply looks up the name and returns the corresponding element.  It
differs from simple list indexing in that it can provide a default value.

}

\value{
\code{parse.control} returns a value for the named highway assignment control parameter

}
\author{
Jeremy Raw
}
\seealso{
	\code{\link{highway.assign}}
}
