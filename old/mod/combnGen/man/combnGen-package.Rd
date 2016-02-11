\name{combnGen-package}
\alias{combnGen-package}
\alias{combnGen}
\docType{package}
\title{
combnGen -- The Combination Generator Package
}
\description{
Provides a method to access combinations by 
use of an index in the range 1:choose(n,k)
}
\details{
\tabular{ll}{
Package: \tab combnGen\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2016-02-10\cr
License: \tab What license is it under?\cr
}
The combnGenGen function initialises and returns
a combnGen function. The generator generator takes
the n and k arguments to produce a generator that will
return a unique combination for a single integer in the
range 1:choose(n,k)
}
\author{
Maintainer: Dale Potter <dale@piratepress.org>
}
\references{

}
combnGenGen
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
> combnGen<-combnGenGen(20,5)
> combnGen(1)
[1] 1 2 3 4 5
> combnGen(choose(20,5))
[1] 16 17 18 19 20
> combnGen(ceiling(runif(1)*choose(20,5)))
[1]  3  4  5 12 19
}
