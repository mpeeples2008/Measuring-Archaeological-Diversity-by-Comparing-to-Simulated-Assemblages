# Bring up dialog to read in a data file. This program expects the file to have types as columns, sites/units as rows with the first column including
# the names of each unit. If your file is in some other format you can modify that before running the remaining lines of code.
x <- read.csv(file=file.choose(),header=T,row.names=1)

# This function has a number of variables that it calls, many of which have default values. x is simply the data file we read in above, nsim is the 
# number of simulated datasets you would like the script to produce (1000 by default), by.d indicates the interval of sample sizes you would like to
# consider when producing random datasets. By default the script produces simulations for every possible sample size between 1 and the maximum. If you 
# have a huge range of values (i.e., 10,000+) you might want to set the interval higher so that it only simulates every other value, for example. 
# Finally, q.d is the quantile you would like to display on the plot. By default, the program displays the 80% quantile (confidence interval) 
# selected by Kintigh in his original study.

div.plot <- function(x, nsim=1000 , by.d=1, q.d=0.8) { # create script and set default values for input
require(vegan) # this script requires the vegan package
maxn <- max(rowSums(x))*1.05 # the number of sample sizes considered is set as the maximum observed sample size + 5%
step.d <- seq(1,maxn,by=by.d) # this step simply creates a sequesnce from 1 to maxn by the interval set using the by.d variable
prob.d <- colSums(x) # this step produces the pool from which random samples will be drawn by simply summing the values across all units by type (column)
divlist <- list() # create an empty list for output
# the following line is the meat of the program we created in class. It uses the rmultinom function to create nsim (1000 by default) simulations by
# randomly drawing from prob.d (the sum of types across all units) for sample size i. i is part of a for loop so this is conducted for every sample
# size from 1 to maxn defined above. Next we use the specnumber command (set to calculate on MARGIN 2 or columns) to calculate the richness of every
# random sample. The results of this procedure are placed into the list divlist which gets longer by 1 each time it is run (length(divlist)+1).
for (i in step.d) {divlist[[length(divlist)+1]] <- specnumber(rmultinom(nsim,i,prob.d),MARGIN=2)} 
mean.d <- rapply(divlist,mean) # this rapply command recursively calculates the mean richness for each sample size in divlist
sd.d <- rapply(divlist,sd) # this rapply command recursively calculates the standard deviation of richness for each sample size in divlist

# the first step is to plot a line (type='l') indicating the mean expected value of richness at each sample size. The xlim and ylim arguments
# specify how tall and wide the plot will be.
plot(step.d,mean.d,type='l',col='red',xlim=c(0,maxn),ylim=c(0,ncol(x)),xlab='Sample Size',ylab='Richness',main=paste(nsim,"simulations"))
# next we add points for the sample size (x axis) against richness (y axis) for the original data
points(rowSums(x),specnumber(x),pch=16)
# now we label the points based on the row names of x. The pos command means the text will be placed to the left of the point. This can be changed
# see ?text for details.
text(rowSums(x),specnumber(x),labels=rownames(x),pos=2,cex=0.5)
# Now we plot lines for the confidence intervals above and below the mean expected richness at each sample size. Unlike Kintigh's (1984) original 
# program, we actually calculate this directly using the following equation (sd.d*qnorm((1-q.d)/2)). This equation multiplys the standard deviation
# by the output of a qnorm quantile function which essentially looks up the critical value we need for a given confidence interval. For an 80% 
# confidence interval we use 1-0.8 = 0.2 and then divide that by 2 to get 0.1. Using the qnorm function to find the appropriate quantile for 
# that value we get -1.28. Thus, this function plot a line for mean.d+sd.d*-1.28 and then mean-sd.d*-1.28. You can find more info on why this works
# here: http://www.statisticshowto.com/probability-and-statistics/find-critical-values/
lines(step.d,mean.d+(sd.d*qnorm((1-q.d)/2)),lty=2,col='blue')
lines(step.d,mean.d-(sd.d*qnorm((1-q.d)/2)),lty=2,col='blue')
# Add a legend indicating the appropriate confidence interval size. This code creates a legend in the top left section of the plot with the labels set
# to read "Mean expected richness" and "X% confidence interval" where X is defined by your q.d variable. The paste command simply adds the value of 
# q.d *100 as test to the output. lty sets the line types for the legend. cex sets the text size. box.lwd=0 argument removes the box around the legend. 
legend('topleft',c('Mean expected richness',paste(q.d*100,'% confidence Interval',sep='')),col=c('red','blue'),lty=c(1,2),cex=0.75,box.lwd=0)}

# once we've got this function defined all we need to do to run it is the following line.
div.plot(x)

# if we want to change some of the default options, we can do that here. In the following example I run 2000 simulations at each sample size and 
# set the confidence interval to 90%
div.plot(x,nsim=2000,q.d=0.9)