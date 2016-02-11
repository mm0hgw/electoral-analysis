\name{combnGen}
\alias{combnGen}

\title{
combnGen
}
\description{
A function returned by combnGenGen(n,k) to generate
combinations over the integer space 1:choose(n,k)
}
\usage{
combnGen(x, ng=n, kg=k)
}

\arguments{
  \item{x}{
an integer in the range 1:choose(n,k)
}
\item{ng}{
used for recursive calls
}
  \item{kg}{
used for recursive calls
}
}
\details{

}
\value{
an integer 'vector' of length(k) consisting of unique,
ordered integer elements in the range 1:n.
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
> combnGen <- combnGenGen(100,15)
> combnGen(1)
 [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
> combnGen(choose(100,15))
 [1]  84  85  86  88  89  91  92  93  94  95  96  97  98  99 100
> combnGen(ceiling(choose(100,15)*runif(1)))
 [1] 10 16 17 23 47 52 61 65 66 67 71 77 83 89 96
> combnGen(ceiling(choose(100,15)*runif(1)))
 [1]  2  8  9 16 28 31 32 40 43 50 56 68 83 87 91
}

\keyword{ ~combn }
\keyword{ ~choose }
\keyword{ ~combnGenGen }

