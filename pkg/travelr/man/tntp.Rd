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

\name{tntp}
\alias{tntp}
\alias{tntp.network}
\alias{tntp.od}
\title{
Load TNTP-format networks
}
\description{
Load publicly available sample networks and trip tables in the TNTP format
}
\usage{
tntp.network(linkfile)
tntp.od(odfile)
}
\arguments{
  \item{linkfile}{
  File containing network links
}
  \item{odfile}{
  File containing origin/destination flow information
}
}
\details{
Use these functions to import sample networks in a standard format originally developed by Hillel Bar-Gera.

Such networks can be found here: \url{http://www.bgu.ac.il/~bargera/tntp}

No function is provided for reading the node information that is sometimes supplied with these networks, since those
files are generally amenable to input through one of the usual \code{\link{read.table}} variants.
}

\value{
\code{tntp.network}returns a \code{\link{highway.net}} structure suitable for use with travelr functions.

\code{tntp.od} returns a matrix of origin/destination flows
}
\author{
Jeremy Raw
Hillel Bar-Gera (TNTP file format)
}
\seealso{
	\url{http://www.bgu.ac.il/~bargera/tntp}
}
