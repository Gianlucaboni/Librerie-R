minquad <- function(x,y,z,assex,assey,titolo,flag) {
	m<-cov(x,y)/var(x)
	q<-mean(y)-m*mean(x)
	varianzam<-1/var(x)*1/sum((1/z)^2)
	varianzaq<-mean(x^2)/(var(x)*sum((1/z)^2))
	gl<-length(x)-2
	Chiquad<-sum((y-m*x-q)^2/z^2)
	fit <- c(M= m, Q = q,varM=varianzam,varQ=varianzaq, GL=gl, CHIQUAD= Chiquad)
	library("plotrix")
	if(flag<=0){
		plotCI(x,y,z,xlab=assex,ylab=assey,main=titolo)
	}else{
		plot(x,y,,xlab=assex,ylab=assey,main=titolo)
	}
	abline(q,m,col="red")
	return(fit)
}
