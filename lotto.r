

getLotto<-function(n=6,g=4){
	x<-NULL
	for(i in 1:n){
			
			r<-sort(c(sample(1:31,6-g),sample(32:49,g)))
			cat(r,"\n")
			x<-c(x,r)
	}
	hist(x,breaks=((0:49)+0.5),col="grey")
	rug(x)
}


getLotto()
