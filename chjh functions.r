library(ggplot2)
library(plyr)

trim.levels <- function(dd) as.data.frame(lapply(dd, function(dd) if(is.factor(dd)) factor(dd) else dd))


one.random.pvalue.per.paper <- function(df){
	
	n.pvals.per.paper <- tapply(df$p.value, df$file.name, length)          
    rows.per.paper <- tapply(1:nrow(df), df$file.name, function(x) x)       # a list with the row numbers belonging to each paper
    rand <- ceiling(runif(length(n.pvals.per.paper) * n.pvals.per.paper)) # Which one of the p values do we randomly sample, for each paper? The first? The second? etc
    chosen.row <- numeric(length(n.pvals.per.paper))
    for(i in 1:length(rand)) chosen.row[i] <- rows.per.paper[[i]][rand[i]] # Pick the relevant rows based on the random numbers
    df <- df[which(1:nrow(df) %in% chosen.row),]
    
	return(df)
}


binomial.bias.test <- function(p, lower.limit=0.03, run.test=T) {

	if(lower.limit==0.03){
		limits = c(0.03, 0.05)
		midpoint <- 0.04
		limits.check(limits)
		p <- p.check(p, limits)
		lower <- length(which(p<midpoint & p>=limits[1]))
		higher <- length(which(p<limits[2] & p>=midpoint))
	} else if(lower.limit==0.04){
		limits = c(0.04, 0.05)
		midpoint <- 0.045
		limits.check(limits)
		p <- p.check(p, limits)		
		lower <- length(which(p<midpoint & p>limits[1]))
		higher <- length(which(p<limits[2] & p>midpoint))
	} else{
		stop("lower.limit must be set to either 0.03 or 0.04")
	}

	if(run.test == T) {r <- binom.test(c(higher,lower), alternative = "greater") 
	    d <- data.frame("lower"=lower, "higher"=higher, "p"=r$p.value)
	    }
	
	else d <- data.frame("lower"=lower, "higher"=higher, "p"=NA) # If we are going to call this function in a bootstrap test, we don't need to actually run the binomial test. Just count up the numbers in the bins, and return those plus a space for the p value.
	
	return(d)
}


binomial.all.test <- function(p, run.test=T) {
	# a two tailed binomial test on the whole dataset

	p <- p.check(p)
	midpoint <- 0.025

	higher <- sum(p>midpoint)
	lower <- sum(p<midpoint)

	if(run.test == T) {r <- binom.test(c(higher,lower)) 
	    d <- data.frame("lower"=lower, "higher"=higher, "p"=r$p.value)
	    }
	else d <- data.frame("lower"=lower, "higher"=higher, "p"=NA)
	
	return(d)
}


limits.check <- function(limits){
	if(!is.numeric(limits)) stop("Your limits for the pcurve.binomial.test function must be numeric")
	if(length(limits)!=2) stop("Your limits for the pcurve.binomial.test function must contain 2 numbers")
	if(limits[1]<0.0) stop("Your lower limit for the pcurve.binomial.test must be >=0.0")
	if(limits[2]>0.05) stop("Your upper limit for the pcurve.binomial.test must be <=0.05")
	if(limits[1]>limits[2]) stop("Your lower limit for the pcurve.binomial.test function must be smaller than your upper limit")
	return(TRUE)
}

p.check <- function(p, limits = c(0.0, 0.05)){
	
	error <- "Your vector of P values must only contain numbers  >=0.0 and <0.05. Please check and try again"
	p <- as.vector(p) # in case of a single number(!)
	limits.check(limits)

	if(is.numeric(p)==FALSE) stop(error)
	if(min(p)<0.00) stop(error)
	if(max(p)>=0.05) stop(error)

	# trim p values outside the limits
	if(limits[1]>0.0) p <- p[p>=limits[1]]
	if(limits[2]<0.05) p <- p[p<=limits[2]]

	if(length(p)==0) stop("No p values fall within the defined limits, cannot complete test")

	return(p)
	
}

run.tests.FoRCode <- function(e, run.test = T){
	
	results <- data.frame("category"=numeric(0), "binomial.bias.p"=numeric(0), "binomial.bias.lower"=numeric(0), "binomial.bias.higher"=numeric(0), "binomial2.bias.p"=numeric(0), "binomial2.bias.lower"=numeric(0), "binomial2.bias.higher"=numeric(0), "binomial.all.p"=numeric(0), "binomial.all.lower"=numeric(0), "binomial.all.higher"=numeric(0))
	
	for(j in unique(as.character(e$FoRCode))){
		b.p <- NA
		a.p <- NA
		c.p <- NA
		
		p.vals <- e$p.value[which(e$FoRCode==j)]
		
		tryCatch(c.p <- binomial.bias.test(p.vals, lower.limit=0.04, run.test = run.test), error = function(e) c.p <- NA)
		tryCatch(b.p <- binomial.bias.test(p.vals, lower.limit=0.03, run.test = run.test), error = function(e) b.p <- NA)
		tryCatch(a.p <- binomial.all.test(p.vals, run.test = run.test), error = function(e) a.p <- NA)
	
	
		if(is.na(c.p)){
			c.p$p <- NA
			c.p$lower <- 0
			c.p$higher <- 0
		}
		if(is.na(b.p)){
			b.p$p <- NA
			b.p$lower <- 0
			b.p$higher <- 0
		}
		if(is.na(a.p)){
			a.p$p <- NA
			a.p$lower <- 0
			a.p$higher <- 0
		}
		
		results[nrow(results)+1,] <- c(as.character(j), b.p$p, b.p$lower, b.p$higher, c.p$p, c.p$lower, c.p$higher, a.p$p, a.p$lower, a.p$higher)
	}
	results$binomial.all.p <- as.numeric(as.character(results$binomial.all.p))   
	results$binomial.all.lower <- as.numeric(as.character(results$binomial.all.lower))
	results$binomial.all.higher <- as.numeric(as.character(results$binomial.all.higher))
	results$binomial.bias.p <- as.numeric(as.character(results$binomial.bias.p))
	results$binomial.bias.lower <- as.numeric(as.character(results$binomial.bias.lower))
	results$binomial.bias.higher <- as.numeric(as.character(results$binomial.bias.higher))
	results$binomial2.bias.p <- as.numeric(as.character(results$binomial2.bias.p))
	results$binomial2.bias.lower <- as.numeric(as.character(results$binomial2.bias.lower))
	results$binomial2.bias.higher <- as.numeric(as.character(results$binomial2.bias.higher))
	results$binomial2.effectsize <- results$binomial2.bias.higher / (results$binomial2.bias.higher + results$binomial2.bias.lower)
	results$binomial.effectsize <- results$binomial.bias.higher / (results$binomial.bias.higher + results$binomial.bias.lower)
	
	binomial.all.effectsize1 <- results$binomial.all.higher / (results$binomial.all.higher + results$binomial.all.lower)
	binomial.all.effectsize2 <- results$binomial.all.lower / (results$binomial.all.higher + results$binomial.all.lower)
	
	results$binomial.all.effectsize <- apply(data.frame(binomial.all.effectsize1, binomial.all.effectsize2), 1, max)

	return(results)
}	


bootstrap.binomial.bias.test <- function(df, replicates){
    lower.limits <- c(0.03, 0.04)
	len <- length(lower.limits)*replicates
	results <- data.frame("lower.limit" = rep(lower.limits, each = replicates), "binomial.bias.lower"=numeric(len), "binomial.bias.higher"=numeric(len), "binomial.bias.p"=numeric(len))
    for(i in 1:2){
    	for(j in 1:replicates) {
    		focal.data <- one.random.pvalue.per.paper(df)
    	    results[(j + replicates*(i-1)), ] <- c(lower.limits[i], binomial.bias.test(focal.data$p.value, lower.limit= lower.limits[i], run.test=F))
    	}
    }
    
    boot.results <- data.frame(lower.limit = lower.limits, binomial.bias.lower = numeric(2), binomial.bias.higher = numeric(2), binomial.bias.p = numeric(2))
	
	for(i in 2:3) boot.results[,i] <- as.numeric(tapply(results[,i], results$lower.limit, mean))
    for(i in 1:2) boot.results[i,4] <- binom.test(c(round(boot.results$binomial.bias.higher[i]), round(boot.results$binomial.bias.lower[i])))$p.value 

    return(boot.results)
}

bootstrap.FoR.test <- function(df, replicates){
	focal.data <- one.random.pvalue.per.paper(df)
	results <- run.tests.FoRCode(focal.data, run.test=F)
	boot.results <- results
	place.holder <- data.frame(category = rep(results$category, replicates-1), binomial.bias.p=NA, binomial.bias.lower=NA, binomial.bias.higher=NA, binomial2.bias.p=NA, binomial2.bias.lower=NA, binomial2.bias.higher=NA, binomial.all.p=NA, binomial.all.lower=NA, binomial.all.higher=NA, binomial2.effectsize=NA, binomial.effectsize=NA, binomial.all.effectsize=NA)
	n.cat <- length(results$category)
	results <- rbind(results, place.holder)
	start <- 1 + n.cat * (2:replicates - 1)
	end <- 2:replicates * n.cat
	
	for(i in 2:replicates){                               # Repeatedly create a dataset with one random p value per paper, run the test on it, and store the result
		focal.data <- one.random.pvalue.per.paper(df)
		results[start[i-1]:end[i-1], ] <- run.tests.FoRCode(focal.data, run.test=F)
	}
	
	results$category <- factor(results$category, levels = results$category[1:n.cat])
	
	for(i in 2:13)	boot.results[,i] <- as.numeric(tapply(results[,i], results$category, mean))    # Find the average number of p values above and below the respective cut offs across all bootstrap replicates. This smoothes out the noise introduced by random sampling of one p value per paper
	
	for(i in 1:nrow(boot.results)){      # Do the approporiate binomial tests on the bootstrapped numbers of p values - NB we need to round off the bootstrap averages to whole numbers for the binomial test
		boot.results[i,"binomial.bias.p"] <- binom.test(c(round(boot.results$binomial.bias.higher[i]), round(boot.results$binomial.bias.lower[i])), alternative = "greater")$p.value 
		boot.results[i,"binomial2.bias.p"] <- binom.test(c(round(boot.results$binomial2.bias.higher[i]), round(boot.results$binomial2.bias.lower[i])), alternative = "greater")$p.value 
		boot.results[i,"binomial.all.p"] <- binom.test(c(round(boot.results$binomial.all.higher[i]), round(boot.results$binomial.all.lower[i])))$p.value 
    }
    
	return(boot.results)
}
	

	