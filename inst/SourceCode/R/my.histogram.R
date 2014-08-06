###  This function creates my histograms
###  Created by Brett Amidan, March 2004

my.histogram <- function(data.vec,nclass=21,trim.val=NULL,trim.text=T,
  bar.labels=F,relative.hist=F,given.xlimits=F,my.col=13,...)  {
	###  data.vec is the vector of data
	###  nclass is the number of class bins to use
	###  trim.val is the highest value wanted on the y axis (NULL uses the max)
	###  trim.text adds the number text to bars that have been trimmed
	###  bar.labels add the number text to each bar
	###  relative.hist is used if proportions are wanted on the y axis
	###  given.xlimits if T then will use the xlim values you have given
	###    for the actual histogram plot (useful if you want similar xaxis
	###    across multiple plots)
	###  my.col is the color of the bars of the histogram
	###  ... means you can add other plot commands to the histogram
	
	### bin the data / need the create.bins function
	temp.bins <- create.bins(data.vec=data.vec,number.bins=nclass)
	
	## create the histogram / need the histogram.bin function
	histogram.bin(bin.low=temp.bins[,1],bin.hi=temp.bins[,2],
    bin.total=temp.bins[,3],trim.val=trim.val,trim.text=trim.text,
    all.text=bar.labels,relative.hist=relative.hist,
    given.xlimits=given.xlimits,my.col=my.col,...)
		
	invisible()
}

### This function will plot a histogram given binned data

histogram.bin <- function(bin.low,bin.hi,bin.total,trim.val=NULL,trim.text=F,
  all.text=F,relative.hist=F,given.xlimits=F,my.col=NA,...)  {
	## bin.low is a vector with the lower bin limits
	## bin.hi is a vector with the upper bin limits
	## bin.total is a vector with the total number in each bin
	## trim.val is a value to trim the histogram to so it doesn't get too tall
	## trim.text will add the number to trimmed bars
	## all.text will add the number to all bars
	## relative.hist gives counts in relative proportions
	## given.xlimits if T then will use the given xlim command
	
	## if a relative histogram is desired
	if (relative.hist)  {
		bin.total <- round(bin.total/sum(bin.total),4)
	}
	
	## if there is no trim value use the max
	if (is.null(trim.val)) {
		y.max <- max(bin.total,na.rm=T)
	}
	else {
		y.max <- trim.val
	}
	number.bins <- length(bin.low)
	## data check
	if (length(bin.low)!=length(bin.hi)) {
		print("WARNING: Not equal bin lengths")
	}
	if (length(bin.low)!=length(bin.total)) {
		print("WARNING: Not equal bin lengths")
	}
	
	## create the plot
	if (given.xlimits) {
		plot(0,0,type="n",ylim=c(0,y.max),...)
	}
	else {
		plot(0,0,type="n",xlim=c(min(bin.low),max(bin.hi)),ylim=c(0,y.max),...)
	}
	## calculate amount of empty space to put between bars
	avg.bin.range <- mean(bin.hi-bin.low)
	bin.space <- number.bins/200*avg.bin.range
	
	## loop thru each bin and add to plot
	for (i in 1:number.bins) {
		if (bin.total[i] > y.max) {
			polygon(c(bin.low[i],bin.low[i],bin.hi[i]-bin.space,bin.hi[i]-bin.space),
        c(0,y.max,y.max,0),density=-1,col=my.col)
			if (all.text) {
				## adds text of number on the bar
        if (relative.hist) {
          text((bin.low[i]+bin.hi[i])/2,y.max,
            as.character(round(bin.total[i],2)),crt=90,adj=c(.5,1),cex=.60)
        }
        if (!relative.hist) {
          text((bin.low[i]+bin.hi[i])/2,y.max,as.character(bin.total[i]),
            crt=90,adj=c(.5,1),cex=.60)
        }
			}
			if (number.bins<40 & trim.text & !all.text)	text((bin.low[i]+bin.hi[i])/2,
        y.max,as.character(bin.total[i]),crt=90,adj=c(.5,1),cex=.60)
		}
		else {
			if (bin.total[i]>0)  polygon(c(bin.low[i],bin.low[i],bin.hi[i]-bin.space,
        bin.hi[i]-bin.space),c(0,bin.total[i],bin.total[i],0),density=-1,
        col=my.col)
			if (all.text) {
				## adds text of number on the bar
        if (relative.hist) {
          text((bin.low[i]+bin.hi[i])/2,bin.total[i],
            as.character(round(bin.total[i],2)),crt=90,adj=c(.5,1),cex=.60)
        }
        if (!relative.hist)  {
          text((bin.low[i]+bin.hi[i])/2,bin.total[i],
            as.character(bin.total[i]),crt=90,adj=c(.5,1),cex=.60)
        }
			}
		}
	}
	invisible()
}

## This file creates bins to be used in histograms

create.bins <- function(data.vec,low.limit=min(data.vec),
  hi.limit=max(data.vec),number.bins=20)  {
	##  data.vec is a vector of the data to be binned
	##  low.limit is the lowest bin marker
	##  hi.limit is the highest bin marker
	##  number.bins is the number of bins desired

	## create the bins
	temp.bins <- seq(low.limit,hi.limit,length=(number.bins+1))
	low.bins <- temp.bins[1:(number.bins)]
	up.bins <- temp.bins[2:(number.bins+1)]

	## create the bin counts output
	bin.totals <- rep(0,number.bins)

	## loop thru the bins and add to the bin totals
	## values on the bin markers are added to bin in which it is on lower marker
	## however, on last bin, values on upper marker will be included
	for (i in 1:number.bins) {
		if (i==number.bins) {
			## last bin to include those = to upper marker
			indy <- data.vec >= low.bins[i] & data.vec <= up.bins[i]
			NULL
		}
		else {
			indy <- data.vec >= low.bins[i] & data.vec < up.bins[i]
			NULL
		}
		bin.totals[i] <- bin.totals[i]+ sum(indy)
	}

	output <- cbind(low.bins,up.bins,bin.totals)
	dimnames(output) <- list(paste("bin",1:length(low.bins),sep=""),
    c("low.bin","up.bin","bin.counts"))
	output
}  ## end of function
