k2k <- function(obj, data, method=NULL, mpower=2, verbose=0){
		   nm <- NULL

		 for(i in obj$mstrataID){
          idx <- which(obj$mstrata==i)
		  tmp <- obj$groups[idx] 
		  tr <- idx[which(tmp==obj$g.names[1])]
		  ct <- idx[which(tmp==obj$g.names[2])]
          n.tr <- length(tr)
          n.ct <- length(ct)
		  m <- min(n.tr,n.ct)
		  n <- n.tr+n.ct
		  if(n.tr != n.ct){
		   idx2 <- c(tr, ct)
		   if(is.null(method)){
		    mat <- matrix(runif(n*n), n,n)
			colnames(mat) <- rownames(data)[idx2]
			rownames(mat) <- rownames(data)[idx2]
			} else {
		     mat <- as.matrix(dist(data[idx2, obj$vars],method=method, p=mpower))
			}
		   m <- min(n.tr, n.ct)
		   mat1 <- matrix(mat[1:n.tr,-(1:n.tr)], n.tr, n.ct)

		   colnames(mat1) <- colnames(mat)[-(1:n.tr)]
		   rownames(mat1) <- rownames(mat)[1:n.tr]
		   if(n.tr > n.ct){
		    for(k in 1:m){
             mins <- apply(mat1, 2, function(x) min(x, na.rm=TRUE))
			 min.c <- min(mins, na.rm=TRUE)
			 col <- which(mins == min.c)[1]
			 row <- which(mat1[,col]==min.c)[1]
			 mat1[row, 1:n.ct] <- NA
			 mat1[1:n.tr ,col] <- NA
			 nm <- c(nm, colnames(mat1)[col], rownames(mat1)[row]) 
			}
		   } else {
		    for(k in 1:m){
             mins <- apply(mat1, 1, function(x) min(x, na.rm=TRUE))
			 min.r <- min(mins, na.rm=TRUE)
			 row <- which(mins == min.r)[1]
			 col <- which(mat1[row,]==min.r)[1]
			 mat1[row, ] <- NA
			 mat1[ ,col] <- NA
			 nm <- c(nm, colnames(mat1)[col], rownames(mat1)[row]) 
			}		   
		   }
		  } else {
		   nm <- c(nm, rownames(obj$X)[c(ct,tr)])
		  } 
		}


idx <- match(nm, rownames(obj$X))
idx <- idx[which(!is.na(idx))]
if(length(idx)>0){
 obj$matched[-idx] <- FALSE
 obj$mstrata[-idx] <- NA
 obj$w <- numeric(dim(data)[1])
 obj$w[idx] <- 1
 obj$k2k <- TRUE
 obj$tab <- cem.summary(obj=obj, verbose=verbose)
}
invisible(obj)
}


