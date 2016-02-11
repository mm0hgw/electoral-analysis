\name{combnGenGen}
\alias{combnGenGen}
\title{
combnGenGen
}
\description{
	Return a combnGen function, which returns
	combinations by integer reference number,
	for the supplied values n and k.
}
\usage{
combnGenGen(n, k)
}

\arguments{
  \item{n}{
number of items from which to choose
}
  \item{k}{
number of items to choose
}
}
\details{

}
\value{
a 'function' which returns unique combinations
as defined by the n and k arguments when supplied
with an integer in the range 1:choose(n,k)
}
\references{

}
\author{
Dale Potter <dale@piratepress.org>
}
\note{

}

\seealso{

}
\examples{
> combnGen<-combnGenGen(100,5)
> combnGen(1)
[1] 1 2 3 4 5
> combnGen(2)
[1] 1 2 3 4 6
> combnGen(choose(100,5))
[1]  96  97  98  99 100
> combnGen(ceiling(runif(1)*choose(100,5)))
[1] 17 56 59 61 99
}

\keyword{ ~combn }
\keyword{ ~choose }
\keyword{ ~combnGen }

