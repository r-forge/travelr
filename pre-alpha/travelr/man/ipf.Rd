\name{ipf}
\alias{ipf}
\alias{print.iterative.fit}
\title{
Iterative proportional fitting
}
\description{
Perform iterative proportional fitting of a matrix
}
\usage{
ipf(mtx, factors, method = NULL, max.rmse = 1e-07, max.iter = 50)
\method{print}{iterative.fit}(x, ...)
}
\arguments{
  \item{mtx}{
  A two-dimensinoal matrix that will be expanded to match the margins contained in \code{factors}
}
  \item{factors}{
  A vector, list, or \code{data.frame} of factors indicating how to grow the matrix
}
  \item{method}{
  How to interpret the factors.  Choices are \dQuote{absolute}, \dQuote{fraction} or \dQuote{percent}.  See details.
}
  \item{max.rmse}{
  Convergence limit.  Terminate fitting when the root mean square error between the expanded array margins and the target
  factors is less than \code{max.rmse}.
}
  \item{max.iter}{
  Convergence limit.  Terminate after this number of row and column iterations if the 
}
  \item{x}{
  An \code{iterative.fit} object returned from \code{ipf} describing convergence results
}
  \item{\dots}{
  Additional arguments to pass on to print methods
}
}
\details{

\code{ipf} will iteratively adjust the matrix margins until they match the corresponding element of the \code{factors}.

The \code{factors} element is either a vector, a list or a \code{data.frame}, with one element or column per dimension
of \code{mtx}.  A vector or two-column \code{data.frame} is appropriate if \code{mtx} is a square matrix, otherwise
\code{factors} must be a list.

The first element of \code{factors} corresponds to the desired row sums of the matrix, and the second element
corresponds to the desired column sums of the matrix.  If \code{factors} is a vector, the same vector will be used to
establish the row and column sums.

The \code{factors} must each be the same length as the corresponding dimension of the input matrix.

The \code{method} parameter indicates how the \code{factors} are interpreted.  The method may be \dQuote{absolute}, \dQuote{fraction},
or \dQuote{percent}.  If \code{method} is \dQuote{absolute}, the factors are taken to be the new margins, and the array is iteratively
expanded until the margins match the new totals.  If \code{method} is \dQuote{fraction} or \dQuote{percent} methods, the supplied
factors are converted to absolute values by multiplying the corresponding row or column sums of the matrix by the
corresponding factors.

Note that the \code{factors} must yield the same grand total for both rows and columns of \code{mtx}.  After the
absolute factors have been prepared according to the chosen \code{method}, if the total of the two sets of growth
factors is not the same, the second growth factor (column) will itself be factored to match the total of the first
growth factor and a warning will be issued.

The print method shows computed array margins and convergence statistics.
}

\value{

Function \code{ipf} returns a matrix with the same dimensions and dimnames as the input matrix, but with a class of
\code{iterative.fit}.  The resulting object has the following attributes:
\describe{
	\item{Converged}{TRUE if max.rmse was achieved}
	\item{RMSE}{The root mean square error after the final iteration}
	\item{Iteration}{How many row and column iterations were executed}
	}
The attributes of the \code{iterative.fit} object are printed along with the matrix margins (building using
\code{\link{addmargins}} when the class-specific print method is invoked.
}

\author{ Jeremy Raw }

\examples{
m<-matrix(c(5,50,100,200,
            50,5,100,300,
            50,100,5,100,
            100,200,250,20),
			nrow=4,ncol=4,byrow=TRUE)
dimnames(m)<-list(Rows=c("R1","R2","R3","R4"),Cols=c("C1","C2","C3","C4"))
print(addmargins(m))
f<-c(400,460,400,702)

mf.vector<-ipf(m,f,"absolute")
print(mf.vector)

f.1<-list(rows=c(400,460,400,702))
mf.list.1<-ipf(m,f.1,"absolute")
print(mf.list.1)

f.2<-list(rows=c(400,460,400,702),cols=c(260,400,500,802))
mf.list.2<-ipf(m,f.2,"absolute")
print(mf.list.2)

df.1<-data.frame(rows=c(400,460,400,702),cols=c(260,400,500,802))
mf.df.1<-ipf(m,df.1,"absolute")
print(mf.df.1)

f.pct<-c(102,105,110,95)
mf.pct<-ipf(m,f.pct,"percent")
print(mf.pct)

}
