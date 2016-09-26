\name{residui}
\alias{residui}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Metodo dei Residui
}
\description{
La funzione applica il Metodo di Residui a un fit lineare per determinare l'icertezza su ogni singolo punto del grafico. Il metodo si applica qual'ora l'incertezza non e' stimabile o se stiamata, poco credibile. Preferibilmente da usare in combinazione con "minquad".
}
\usage{
residui(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	vettore numerico contenente le ascisse dei punti da esaminare (ha la stessa dimensione del vettore y).
}
  \item{y}{
%%     	vettore numerico contenete le ordinate dei punti da esaminare (ha la stessa dimensione del vettore x).
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
	Gianluca Boni
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
	x=c(1,4,5,7)
	y=c(3,11,12,21)
	residui(x,y)
## The function is currently defined as
function (x, y) 
{
    m <- cov(x, y)/var(x)
    q <- mean(y) - m * mean(x)
    sigma2 <- sum((y - m * x - q)^2)/(length(x) - 2)
    return(sqrt(sigma2))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
