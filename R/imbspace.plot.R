plot.imbalance.space <- function(...) imbspace.plot(...)

imbspace.plot2 <- function(obj,group="1"){
    #if(class(obj) != "imbalance.space")
    if(!inherits(obj,"imbalance.space"))
	stop("obj must be of class `imbalance.space'")
  imE <- new.env()
  imE$obj <- obj
  obj <- NULL
	g <- sprintf("G%s",group)
	imE$n <-  imE$obj$space[[g]]
	imE$ML1 <- imE$obj$space$ML1
	imE$Relaxed <- imE$obj$space$Relaxed
	imE$class <- rep("relax", length(imE$n))
	id.raw <- which(imE$Relaxed=="<raw>")
	id.start <- which(imE$Relaxed=="<start>") 
	imE$class[ id.raw ] <- "raw"
	if(length(id.start)>0)
	  imE$class[ id.start ] <- "start"
	ids <- (1:length(imE$n))[-c(id.raw, id.start)]
	name.vars <- names(imE$obj$coars[[1]])
	n.vars <- length(name.vars)
	tab <- data.frame(n=imE$n, ML1=imE$ML1, class=imE$class)
	
	dotcol <- rgb(0.2,0.2,0.8)
	selcol <- rgb(0.8,0.8,0.2)
	main.txt <- sprintf("Total units=%d, ML1=%.3f", imE$n[id.raw], imE$ML1[id.raw])
	if(length(id.start)>0)
	 main.txt <- sprintf("Initial matched units=%d, ML1=%.3f", imE$n[id.start], 
	                     imE$ML1[id.start])
	
	plot(1/sqrt(imE$n[ids]), imE$ML1[ids],
         xlab="number matched, scaled as 1/sqrt(matched)", 
		 ylab="median of L1 profile", 
         pch=20, col=dotcol, ylim=range(imE$ML1), xlim=range(1/sqrt(imE$n)),
         main=main.txt,
		 axes=FALSE)
	axis(2)
	x1 <- pretty( 1/sqrt(imE$n), 5)
	axis(1, x1, round(1/x1^2))
	box()
	points(1/sqrt(imE$n[id.raw]), imE$ML1[id.raw],   col="red", pch=20)
	text(1/sqrt(imE$n[id.raw]), imE$ML1[id.raw], "raw",  col="red",adj=-0.5, cex=0.7)
	if(length(id.start)>0){
		points(1/sqrt(imE$n[id.start]), imE$ML1[id.start],   col="green", pch=20)
		text(1/sqrt(imE$n[id.start]), imE$ML1[id.start], "start",  col="green",
			  adj=-0.5, cex=0.7)
	}

}



print.selected.cem <- function(x, ...){
	print(x$breaks)
}






imbspace.plot <- function(obj,group="1", data, explore=TRUE, verbose=1){
	if(!interactive() | !explore){
		imbspace.plot2(obj, group)
		return(NULL)	
	}
	
	
#	if(class(obj) != "imbalance.space")
    if(!inherits(obj,"imbalance.space"))
	stop("obj must be of class `imbalance.space'")
	imE <- new.env()
	imE$obj <- obj
	obj <- NULL
	
	haveTCL <- interactive()
	if(!capabilities("tcltk")){
		haveTCL <- FALSE	
		warning("\ntcltk support is absent")
	}
	
	if(haveTCL){
		have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
		if(have_ttk) {
			tkbutton <- ttkbutton
			tkframe <- ttkframe
			tklabel <- ttklabel
			tkradiobutton <- ttkradiobutton
		}
	}
	
	g <- sprintf("G%s",group)
	imE$n <-  imE$obj$space[[g]]
	imE$ML1 <- imE$obj$space$ML1
	imE$Relaxed <- imE$obj$space$Relaxed
	imE$class <- rep("relax", length(imE$n))
	id.raw <- which(imE$Relaxed=="<raw>")
	id.start <- which(imE$Relaxed=="<start>") 
	imE$class[ id.raw ] <- "raw"
	if(length(id.start)>0)
	  imE$class[ id.start ] <- "start"
	ids <- (1:length(imE$n))[-c(id.raw, id.start)]
	name.vars <- names(imE$obj$coars[[1]])
	n.vars <- length(name.vars)
	tab <- data.frame(n=imE$n, ML1=imE$ML1, class=imE$class)
	main.txt <- sprintf("Total units=%d, ML1=%.3f", imE$n[id.raw], imE$ML1[id.raw])
	
	if(length(id.start)>0)
		main.txt <- sprintf("Initial matched units=%d, ML1=%.3f", imE$n[id.start], imE$ML1[id.start])
	
	dotcol <- rgb(0.2,0.2,0.8)
	selcol <- rgb(0.8,0.8,0.2)
	plot(1/sqrt(imE$n[ids]), imE$ML1[ids],
         xlab="number matched, scaled as 1/sqrt(matched)", 
		 ylab="median of L1 profile", 
         pch=20, col=dotcol, ylim=range(imE$ML1), xlim=range(1/sqrt(imE$n)),
         main=main.txt, axes=FALSE)
	axis(2)
	x1 <- pretty( 1/sqrt(imE$n), 5)
	axis(1, x1, round(1/x1^2))
	box()
	points(1/sqrt(imE$n[id.raw]), imE$ML1[id.raw],   col="red", pch=20)
	text(1/sqrt(imE$n[id.raw]), imE$ML1[id.raw], "raw",  col="red",adj=-0.5, cex=0.7)
	if(length(id.start)>0){
		points(1/sqrt(imE$n[id.start]), imE$ML1[id.start],   col="green", pch=20)
		text(1/sqrt(imE$n[id.start]), imE$ML1[id.start], "start",  col="green",adj=-0.5, cex=0.7)
	}
	idx.sav <- id.raw
	if(length(id.start)>0)
		idx.sav <- id.start

	old.idx <- NULL
	imE$xy <- xy.coords(1/sqrt(imE$n), imE$ML1)
	
	imE$tmp.br <- imE$obj$coars[[2]]
	if(length(id.start)>0)
	  imE$tmp.br <- imE$obj$coars[[id.start]]
	imE$new.br <- imE$tmp.br
	
	goOn <- TRUE
	
	if(haveTCL){	
		tclServiceMode(FALSE)
		tt <- tktoplevel()
		
		tkwm.title(tt,"Modify CEM solution")
		entries <- list()
		tcvars <- list()
		infoText <- tclVar( sprintf("Total units=%d, ML1=%.3f", imE$n[id.raw], imE$ML1[id.raw]) )
		if(length(id.start)>0)
			infoText <- tclVar( sprintf("Matched units=%d, ML1=%.3f", imE$n[id.start],imE$ML1[id.start]) )
		label1 <- tklabel(tt, textvariable=infoText) 
		tkpack(label1)
		
		imE$n.tmp.br <- length(imE$tmp.br)
		for(i in 1:imE$n.tmp.br){
			
			tcvars[[i]] <- tclVar( deparse( round(imE$tmp.br[[i]], 2), width.cutoff=500) )  
			entries[[i]] <- tkentry(tt, width="100", textvariable=tcvars[[i]])	    
			
			tkpack( tklabel(tt, text=sprintf("Variable: %s", names(imE$tmp.br)[i]) ))
			tkpack(  entries[[i]] )
		}

		tcvars[[imE$n.tmp.br+1]] <- tclVar(  )  
		entries[[imE$n.tmp.br+1]] <- tkentry(tt, width="100", textvariable=tcvars[[imE$n.tmp.br+1]])	    
		
		tkpack( tklabel(tt, text="Additional CEM args:"))
		tkpack(  entries[[imE$n.tmp.br+1]] )
		
		
		OnOK <- function(){
			other.args <- NULL
			if(verbose>=1)
			  cat("\n... running new cem...\n")
			imE$n.tmp.br <- length(imE$tmp.br)
			for(i in 1:imE$n.tmp.br){
				vv <- names(imE$tmp.br)[i]
				tmpc <- tclvalue( tcvars[[i]] )
				imE$new.br[[i]] <- try(eval(parse(text=tmpc)), silent = TRUE)
                #if( class(imE$new.br[[i]]) == "try-error"){
                if( inherits(imE$new.br[[i]],"try-error")){
					warning(sprintf("\nError in settings cutpoints of variable << %s >>:\n\n >> %s <<\n\n Using original ones.\n", vv, tmpc) )
				  imE$new.br[[i]] <- imE$tmp.br[[i]] 
				}
			}
			tmpc <- tclvalue( tcvars[[imE$n.tmp.br+1]] )
			other.args <- try(eval(parse(text=tmpc)), silent = TRUE)
            #if( class(other.args) == "try-error"){
            if( inherits(other.args, "try-error")){
				warning(sprintf("\nError in additional CEM arguments specification. Ignoring them.\n", tmpc) )
				other.args <- NULL 
			} else 
			 other.args <- tmpc
			tclServiceMode(FALSE)	 
			if(!is.null(other.args))
			 eval(parse(text=sprintf("tmp.mat <- cem(imE$obj$match$treatment, data=data[,c(imE$obj$match$vars,imE$obj$match$treatment) ], cutpoints=imE$new.br, eval.imbalance=FALSE, %s)", other.args)))
		    else
			   tmp.mat <- cem(imE$obj$match$treatment, data=data[,c(imE$obj$match$vars,imE$obj$match$treatment) ], cutpoints=imE$new.br, eval.imbalance=FALSE)
			   
			tclServiceMode(TRUE)
			tmp.ML1 <- L1.meas(imE$obj$match$groups, data=data[,imE$obj$match$vars], breaks=imE$obj$medianCP, weights=tmp.mat$w)$L1
			tmp.n <- tmp.mat$tab["Matched", g] 
			len.c <- length(imE$n)
			imE$n[len.c+1] <- tmp.n 	 
			imE$ML1[len.c+1] <- tmp.ML1 	 
			imE$Relaxed[len.c+1] <- "<new>"	 
			imE$obj$coars[[len.c+1]] <- imE$new.br	
			imE$xy <- xy.coords(1/sqrt(imE$n), imE$ML1)
			update.imbplot(len.c+1)	 
		}
		
		
		OK.but <-tkbutton(tt,text="   Run CEM with these coarsening   ",command=OnOK)
		
		
		tkpack(OK.but) 

		tclServiceMode(TRUE)
	}
	
	firstime <- TRUE
	update.imbplot <- function(mT){
		if(length(mT)==0)
		return(FALSE)
		
		
		idx <- which(imE$n>=imE$n[mT])
		idx2 <- which(imE$ML1[idx]<= imE$ML1[mT])
		idx <- idx[idx2]
		
		id.new <- which(imE$Relaxed =="<new>")
		
		points(1/sqrt(imE$n[idx.sav]), imE$ML1[idx.sav],   col=dotcol, pch=20)
		points(1/sqrt(imE$n[idx]), imE$ML1[idx],   col=selcol, pch=20)
		points(1/sqrt(imE$n[id.raw]), imE$ML1[id.raw],   col="red", pch=20)
		points(1/sqrt(imE$n[mT]), imE$ML1[mT],   col="orange", pch=20)
		if(length(id.start)>0){
			points(1/sqrt(imE$n[id.start]), imE$ML1[id.start],   col="green", pch=20)
			text(1/sqrt(imE$n[id.start]), imE$ML1[id.start], "start",  col="green",adj=-0.5, cex=0.7)
		}
		if(length(id.new))
		 points(1/sqrt(imE$n[id.new]), imE$ML1[id.new],   col="cyan", pch=20)
		id.bad <- which(idx == id.raw)
		if(length(id.bad>0))
		 idx <- idx[-id.bad]
		if(length(idx)>0){
 		  y <- lapply(imE$obj$coars[idx], function(a) unlist(lapply(a, length)))
		  x <- matrix(unlist(y), length(y), length(y[[1]]), byrow=TRUE) 
		
		  colnames(x) <- names(y[[1]])
		  tmp <- as.data.frame(x)
				
		  tmp2 <- data.frame(tmp, ML1=imE$ML1[idx])	
		  rownames(tmp2) <- idx
		
		 if(haveTCL & !is.null(imE$obj$coars[[mT]])){
			ttt <- imE$obj$coars[[mT]]
 			for(i in 1:length(imE$tmp.br)){
				tclvalue( tcvars[[i]] )  <-  deparse( round(ttt[[i]], 2), width.cutoff=500)
  	 		}

			tclvalue(infoText) <- sprintf("Matched units=%d, ML1=%.3f", imE$n[mT], imE$ML1[mT]) 

 			tkfocus(tt)	
		 }
		}

		
		imE$old.idx <- list(breaks = imE$obj$coars[[mT]], n=imE$n[mT], ML1=imE$ML1[mT], medianCP=imE$obj$medianCP)
	
		
		imE$idx.sav <- idx
		
		return(TRUE)
		
	}
	
	imE$old.idx <- NULL
	update.imbplot( 2 )
	
	while(goOn){
	  imE$xy <- xy.coords(1/sqrt(imE$n), imE$ML1)
		goOn <- update.imbplot(identify(imE$xy, n=1,plot=FALSE)) 
	}
	
    if(haveTCL)
	 tkdestroy(tt)
	
	
	class(imE$old.idx) <- "selected.cem"
	return(imE$old.idx)
}

