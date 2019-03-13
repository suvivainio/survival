setwd("//ad.helsinki.fi/home/s/suvivain/Documents/2019 Survival analysis")
library(survival)
?veteran # for explanation of the variables in the data set
data(veteran) # load the data
str(veteran) # show records of the data
view(veteran)

# 1. Plot a histogram of the survival times corresponding to uncensored observations (veteran$status == 1)
hist(veteran$time[veteran$status == 1], nclass=30, main='', xlab='Survival time (days)',
     col='gray90')
hist(veteran$time[veteran$status == 1], main='', xlab='Survival time (days)',
     col='gray90')
# Right skewed data

# 2. . Create an output file where the histogram is stored. Remember to first defint 
# outpath where the file should be stored.

pdf(file.path(getwd(), 'survtimes.pdf'))
op <- par(mar=c(4,4,0,0), mgp=c(2,1,0))
hist(veteran$time[veteran$status == 1], nclass=30, main='', xlab='Survival time (days)',
     col='gray90')
par(op)
dev.off()

# 3. a. Use the survifit routine in R to calculate the Kaplan-Meier estimate of the overall survival in the
# data. In the survival routines of R, the response variable needs to be specified as a survival object.
# If the observed failure time variable is time and failure indicator variable is status, the response
# variable is created as
head(veteran)
analyse1=survfit(Surv(time, status)~1, data=veteran) #, conf.type="none")
summary(analyse1)
plot(analyse1, col="red")
# Change confidence interval  
analyse1=survfit(Surv(time, status)~1, data=veteran, conf.int=0.8)
lines(analyse1, col="blue")
legend("Blue: 95 % CI", "Red: 80 %")

# 3. b. Plot the Kaplan-Meier estimates of the survival functions separately for the two treatment groups
# (standard vs. test). Does there appear to be a difference between the two groups in survival?
analyse2=survfit(Surv(time, status)~trt, data=veteran)#, conf.type="none")
plot(analyse2, col=c("blue", "red"))
legend("topright", legend = c("standard", "test"), col=c("blue", "red"), lty=1)

#  Irrespective of the treatment group, compare the survival in groups defined by the histological
# type of tumor (variable celltype). You may also like to explore the effect on survival of the other
# covariates in the data.
analyse3=survfit(Surv(time, status)~celltype, data=veteran, conf.int=0.8)#, conf.type="none")
summary(analyse2)
plot(analyse3, col=c("red", "blue", "green", "purple"))
legend("topright", legend=levels(veteran$celltype), col=c("red", "blue", "green", "purple"), lty=1)
print(analyse3)

# 

