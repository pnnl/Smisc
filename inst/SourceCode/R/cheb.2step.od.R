### This function allows for a 2-step Chebyshev outlier detection method that can find multiple outliers
###  Created by Brett Amidan - April 2001, updated to use second tail March 2003
### Modified by J Hathaway, 2006-11
### Modified by Landon Sego, 2007-02-01
### Added one-sided unimodal functionality

cheb.2step.od <- function(the.vector, unimodal = TRUE, first.rejval = 1/1000,
	                  second.rejvals = c(1/100,1/1000,1/10000),
                          do.plot = FALSE, zero.var.correction = TRUE,
                          one.sided = NULL,...){

#        ... are additional arguments to my.histogram


	## First step using Chebyshev to determine which samples should be included in the calculations for 
	## the second step;  this identifies possible outliers and gives them the value of 1 + the max value not identified
	## as an outlier and includes them in the st. deviation calculation for step 2
	
	
	out.step1 <- cheb.od(the.data=the.vector,
                             unimodal=unimodal,
                             rej.val=first.rejval)

        # Two sided
        if (is.null(one.sided)) {
          if (!is.null(out.step1)){
            step1.lo.cv <- out.step1[1,"lower.cv"]
            step1.hi.cv <- out.step1[1,"upper.cv"]
            outlier <- (the.vector < step1.lo.cv) | (the.vector > step1.hi.cv)
  	  }
          else 
            outlier <- rep(FALSE,length(the.vector))
        }

        # If one-sided
        else {
          if (!(tolower(one.sided) %in% c("upper","lower")))
            stop("'one.sided' must be 'NULL', 'upper', or 'lower'")

          if (!is.null(out.step1)){

            if (tolower(one.sided) == "upper") {
              step1.hi.cv <- out.step1[1,"one.sided.upper.cv"]
              outlier <- (the.vector > step1.hi.cv)

            }
            else {
              step1.lo.cv <- out.step1[1,"one.sided.lower.cv"]
              outlier <- (the.vector < step1.lo.cv)              
            }
  	  }
          else 
            outlier <- rep(FALSE,length(the.vector))
        }
        
	## finds all data points in rejection region
	
	new.data <- the.vector[!outlier]

	removed.step1 <- the.vector[outlier]


	### This step takes care of a zero variance problem that can occur in step 1 if all 
	### values not removed in the first step are the same
	########  This takes values removed in step 1 and replaces them with the critical value in step 1 and
	########  it changes the first.rejval to 1/length(the.vector) for step 1
	
	if (zero.var.correction & var(new.data)==0)  {

		out.step1 <- cheb.od(the.data=the.vector,
                                     unimodal=unimodal,
                                     rej.val=1/length(the.vector))
                # if two sided
                if (is.null(one.sided)) {

        	  step1.lo.cv <- out.step1[1,"lower.cv"]
		  step1.hi.cv <- out.step1[1,"upper.cv"]

                  ## finds all data points in upper rejection region
		  outlier <- the.vector > step1.hi.cv
		  new.data <- the.vector
		  new.data[outlier] <- step1.hi.cv
		  removed.step1 <- the.vector[outlier]

		  ## finds all data points in lower rejection region
		  outlier <- new.data < step1.lo.cv
		  removed.step1 <- c(removed.step1,new.data[outlier])
		  new.data[outlier] <- step1.lo.cv
                }

                # if one sided
                else {

                  if (tolower(one.sided) == "upper") {

                    ## finds all data points in upper rejection region
                    step1.hi.one <- out.step1[1,"one.sided.upper.cv"]
                    outlier <- the.vector > step1.hi.one
                    new.data <- the.vector
  		    new.data[outlier] <- step1.hi.one

                  }
                  else {

                    ## finds all data points in upper rejection region
                    step1.lo.one <- out.step1[1,"one.sided.lower.cv"]
                    outlier <- the.vector < step1.lo.one
                    new.data <- the.vector
  		    new.data[outlier] <- step1.lo.one

                  }                  

  		  removed.step1 <- the.vector[outlier]
                  
                }                
	}
	
	######################################################
	## step 2 using new.data (those not rejected in step1)

	out.step2 <- cheb.od(the.data=new.data,
                             unimodal=unimodal,
                             rej.val=second.rejvals)

	#################################################
	## does a histogram with cutoff lines
	if (do.plot)  {

          # if two sided
          if (is.null(one.sided)) {

                my.histogram(data.vec=the.vector,
                             trim.text=TRUE,nclass=21,ylab="Count",given.xlimits=TRUE,
			     xlim=c(min(the.vector,out.step2[,"lower.cv"],na.rm=TRUE),
                             max(the.vector,out.step2[,"upper.cv"],na.rm=TRUE)),...)
		abline(v=out.step1[,"lower.cv"],col=8)
		abline(v=out.step1[,"upper.cv"],col=8)
		abline(v=out.step2[,"lower.cv"],col=4)
		abline(v=out.step2[,"upper.cv"],col=4)

          } # if two sided

          # if one sided
          else {

            if (tolower(one.sided) == "upper") {
	        my.histogram(data.vec=the.vector,
                             trim.text=TRUE, nclass=21, ylab="Count", given.xlimits=TRUE,
			     xlim=c(min(the.vector, na.rm=TRUE),
                                    max(the.vector, out.step2[,"one.sided.upper.cv"], na.rm=TRUE)),
                             ...)
		abline(v=out.step1[,"one.sided.upper.cv"], col=8)
		abline(v=out.step2[,"one.sided.upper.cv"], col=4)
            }
            else {
	        my.histogram(data.vec=the.vector,
                             trim.text=TRUE, nclass=21, ylab="Count", given.xlimits=TRUE,
			     xlim=c(min(the.vector, out.step2[,"one.sided.lower.cv"], na.rm=TRUE),
                                    max(the.vector, na.rm=TRUE)),
                             ...)
		abline(v=out.step1[,"one.sided.lower.cv"], col=8)
		abline(v=out.step2[,"one.sided.lower.cv"], col=4)
            }

          } # if one sided
          
	} # if (do.plot)
	
	## output
        if (is.null(one.sided)) 
    	  return(list(output=matrix(out.step2[,c("probabilities","lower.cv","upper.cv")], ncol=3,
                                    dimnames=list(NULL,c("probabilities","lower.cv","upper.cv"))),
                      removed.in.step1=removed.step1,
                      step1.cv=out.step1[,c("probabilities","lower.cv","upper.cv")]))

        else if (tolower(one.sided) == "upper")
    	  return(list(output=matrix(out.step2[,c("probabilities","one.sided.upper.cv")], ncol=2,
                                    dimnames=list(NULL,c("probabilities","one.sided.upper.cv"))),                      
                      removed.in.step1=removed.step1,
                      step1.cv=out.step1[,c("probabilities","one.sided.upper.cv")]))

        else 
    	  return(list(output=matrix(out.step2[,c("probabilities","one.sided.lower.cv")], ncol=2,
                                    dimnames=list(NULL,c("probabilities","one.sided.lower.cv"))), 
                      removed.in.step1=removed.step1,
                      step1.cv=out.step1[,c("probabilities","one.sided.lower.cv")]))
        
} # end cheb.2step.od

