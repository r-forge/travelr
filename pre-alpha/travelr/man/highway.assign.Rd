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

\name{highway.assign}
\alias{highway.assign}
\alias{highway.assignment}
\alias{highway assignment}
\title{
Perform highway assignment using a variety of methods
}
\description{
Perform highway assignment using a variety of methods such as
All-or-Nothing (AON), Multiple Successive Averages (MSA), or
equilibrium approaches such as the Frank-Wolfe linearized convex
solution strategy, or the ParTan variation of the Frank-Wolfe algorithm.
}
\usage{
highway.assign(aset, method = c("AON", "MSA", "Frank.Wolfe", "ParTan" ), control=vector("list",0))
}
\arguments{
  \item{aset}{
  An assignment set whose classes will be assigned.
}
  \item{method}{
  The assignment method, which should be one of \dQuote{\acronym{AON}}, \dQuote{\acronym{MSA}}, \dQuote{Frank.Wolfe} or \dQuote{ParTan}; if no method is specified, then \dQuote{\acronym{AON}}
}
  \item{control}{
  Parameters for the assignment; varies by method, see details
}
}
\details{
Details about Parameters
Description of each algorithm, its control parameters and what it returns
Description of intercept control parameter and results
Description of log control parameter and results
}
\value{
Returns a list with various elements; see details
}
\author{
Jeremy Raw
}
\seealso{
\code{\link{assignment.set}}, \code{\link{parse.control}}, \code{\link{assignment.functions}}

For a short complete example: \code{example{\link{skim.paths}}}
}
