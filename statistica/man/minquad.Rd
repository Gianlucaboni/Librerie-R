\name{minquad}
\alias{minquad}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Metodo dei Minimi Quadrati
}
\description{
La funzione fornisce una regressione lineare adottando il Metodo dei Minimi Quadrati, calcolando tutti i parametri utili con relative incertezze. 
}
\usage{
minquad(x, y, z, assex, assey, titolo, flag)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	vettore contente le ascisse dei punti registrati  (deve essere della stessa dimensione del vettore x).
}
  \item{y}{
	vettore contente le ordinate dei punti registrati (deve essere della stessa dimensione del vettore x).
}
  \item{z}{
	vettore che contiene le incertezze dei punti rappresentati. Se non sono stimabili inserire 0.
}
  \item{assex}{
	vettore di caratteri indicante la grandezza espressa lungo l'asse X.
}
  \item{assey}{
	vettore di caratteri indicante la grandezza espressa lungo l'asse Y.
}
  \item{titolo}{
	vettore di caratteri indicante il titolo del gafico.
}
  \item{flag}{
	numero che determina la rappresentazione o meno della barra di errore. Se "flag"<=0 verranno rappresentate le barre, altrimenti no.
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
	x=c(1,2,3,4)
	y=c(2.7,5,7,9.2)
	errorbars=rnorm(4,0,2)
	assex="Tempo [s]"
	assey="Velocita' [m/s]"
	titolo="Andamento della Velocita' in funzione del Tempo"
	flag=0
	minquad(x,y,errorbars,assex,assey,titolo,flag)
## The function is currently defined as
function (x, y, z, assex, assey, titolo, flag) 
{
    m <- cov(x, y)/var(x)
    q <- mean(y) - m * mean(x)
    varianzam <- 1/var(x) * 1/sum((1/z)^2)
    varianzaq <- mean(x^2)/(var(x) * sum((1/z)^2))
    gl <- length(x) - 2
    Chiquad <- sum((y - m * x - q)^2/z^2)
    fit <- c(M = m, Q = q, varM = varianzam, varQ = varianzaq, 
        GL = gl, CHIQUAD = Chiquad)
    library("plotrix")
    if (flag <= 0) {
        plotCI(x, y, z, xlab = assex, ylab = assey, main = titolo)
    }
    else {
        plot(x, y, , xlab = assex, ylab = assey, main = titolo)
    }
    abline(q, m, col = "red")
    return(fit)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
