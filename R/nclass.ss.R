# Adapted by Stefano M. Iacus from 
# Neural Computation 2007 19:6, 1503-1527
nclass.ss <- function(x){
	N <- 2:100
	C <- numeric(length(N))
	D <- C
	for (i in 1:length(N)) {
		D[i] <- diff(range(x))/N[i]
		edges = seq(min(x),max(x),length=N[i])
		hp <- hist(x, breaks = edges, plot=FALSE )
		ki <- hp$counts
		k <- mean(ki)
		v <- sum((ki-k)^2)/N[i]
		C[i] <- (2*k-v)/D[i]^2	#Cost Function
	}
	idx <- which.min(C)
    N[idx]
}