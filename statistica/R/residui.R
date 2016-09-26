residui <-function(x,y) {
	m<-cov(x,y)/var(x); q<-mean(y)-m*mean(x)
	sigma2<-sum((y-m*x-q)^2)/(length(x)-2)
	return(sqrt(sigma2))
}
