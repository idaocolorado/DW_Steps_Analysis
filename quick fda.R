library(fda)



fitbit <- read.csv("C:/Users/weitzend/Documents/Big Data Working Group/IDAO_Correlation_Continuous-master/Mike_Fitbit_data.csv", as.is = TRUE, header = TRUE)
fitbit$Date <- as.Date(fitbit$Date)
fitbit$timetotal <- fitbit$Minutes.Sedentary + fitbit$Minutes.Lightly.Active + fitbit$Minutes.Fairly.Active + fitbit$Minutes.Very.Active + fitbit$Time.in.Bed
fitbit$perc.recorded <- fitbit$timetotal/(max(fitbit$timetotal, na.rm=TRUE))
fitbit <- subset(fitbit, fitbit$perc.recorded >= 0.7)
fitbit$sleepquality <- fitbit$Minutes.Asleep/fitbit$Time.in.Bed
attach(fitbit)
data <- as.data.frame(cbind(fitbit$Minutes.Asleep, fitbit$Steps, fitbit$Minutes.Very.Active))
colnames(data) <- c("Sleep", "Steps", "VeryActive")

plot(1:211,data$Steps,ylab="Steps",pch=16,xlab="Days")
points(1:211,rep(mean(data$Steps),211),type="l",lwd=2)

plot(1:211,data$Steps,ylab="Steps",pch=16,xlab="Days")
for (i in 0:6)
{
  start <- i * 30
  points((1+start):(30+start),rep(mean(data$Steps[(1+start):(30+start)]),30),type="l",lwd=2)
}

plot(1:211,data$Steps,ylab="Steps",pch=16,xlab="Days")
for (i in 0:30)
{
  start <- i * 7
  points((1+start):(7+start),rep(mean(data$Steps[(1+start):(7+start)]),7),type="l",lwd=2)
}


######################################
### B-spline basis ###################
######################################

getB <- function(nbasis=20,norder=4,var=data$Steps)
{
  n <- length(var)
  argvals <- seq(0,1,len=n)
  basisobj <- create.bspline.basis(c(0,1),nbasis,norder)
  ys <- smooth.basis(argvals=argvals, y=var, fdParobj=basisobj)
  results <- list(fd=ys$fd,basis=basisobj,argvals=argvals)
  results
}

######################################
### FOURIER basis ####################
######################################

getF <- function(nbasis=18,var=data$Steps)
{
  fbasisobj <- create.fourier.basis(c(0,length(var)),nbasis)
  ffd <- smooth.basis(y=var,fdParobj=fbasisobj)
  results <- list(ffd=ffd,basis=fbasisobj)
  results
}

#############################
outcome <- data$Steps
#outcome <- data$Sleep
#outcome <- data$VeryActive
#############################
bspl <- getB(nbasis=20,norder=4,var=outcome)
xfd <- bspl$fd
basisobj <- bspl$basis
argvals <- bspl$argvals
#############################
fcurv <- getF(nbasis=18,var=outcome)
ffd <- fcurv$ffd
fbasisobj <- fcurv$basis
#############################

#About a month of data
par(mfrow=c(2,1))
plot(basisobj,xlim=c(0,(30/210)),lwd=2)
plot(fbasisobj,xlim=c(0,30),lwd=2)
par(mfrow=c(1,1))

#The whole period
par(mfrow=c(2,1))
plot(basisobj)
plot(fbasisobj)
par(mfrow=c(1,1))

par(mfrow=c(2,1))
#  Plot the curve along with the data
plotfit.fd(outcome, argvals, xfd,sub="",ylab="Steps")
plot(ffd,ylim=c(0,27000),ylab="Steps")
#plotfit.fd(outcome, argvals, xfd,sub="",ylab="Sleep")
#plot(ffd,ylim=c(0,600),ylab="Sleep")
#plotfit.fd(outcome, argvals, xfd,sub="",ylab="Very Active Minutes")
#plot(ffd,ylim=c(0,150),ylab="Very Active Minutes")
points(1:211,outcome)
par(mfrow=c(1,1))


ffd[[1]][[1]]



#########################
### create new data #####
#########################

theData <- cbind (data,1:nrow(data))
names(theData)[4] <- "WeekDay"
theData$WeekDay <- theData$WeekDay %% 7 + 1
for (i in 1:5)
{
  newCase <- theData
  newCase$Sleep <- trunc(newCase$Sleep + rnorm(nrow(theData),0,sd(newCase$Sleep)))
  newCase$Steps <- trunc(newCase$Steps + rnorm(nrow(theData),0,sd(newCase$Steps)))
  newCase$VeryActive <- trunc(newCase$VeryActive + rnorm(nrow(theData),0,sd(newCase$VeryActive)))
  newCase$id <- rep(i,nrow(newCase))
  newCase$grp <- rep(1,nrow(newCase))
  
  if (i == 1)
  {
    grpData <- newCase
  }
  else
  {
    grpData <- rbind(grpData,newCase)
  }
}
for (i in 6:10)
{
  newCase <- theData
  newCase$Sleep <- trunc(newCase$Sleep + rnorm(nrow(theData),0,sd(newCase$Sleep))) + rnorm(nrow(theData),0.5 * sd(newCase$Sleep))
  newCase$Steps <- trunc(newCase$Steps + rnorm(nrow(theData),0,sd(newCase$Steps)))+ rnorm(nrow(theData),0.5 * sd(newCase$Steps))
  newCase$VeryActive <- trunc(newCase$VeryActive + rnorm(nrow(theData),0,sd(newCase$VeryActive)))+ rnorm(nrow(theData),0.5 * sd(newCase$VeryActive))
  newCase$id <- rep(i,nrow(newCase))
  newCase$grp <- rep(1,nrow(newCase))
  
  grpData <- rbind(grpData,newCase)
}



theData <- cbind (data,1:nrow(data))
names(theData)[4] <- "WeekDay"
theData$WeekDay <- theData$WeekDay %% 7 + 1
for (i in 1:5)
{
  newCase <- theData
  newCase$Sleep <- trunc(newCase$Sleep + rnorm(nrow(theData),0,sd(newCase$Sleep)))
  newCase$Steps <- trunc(newCase$Steps + rnorm(nrow(theData),0,sd(newCase$Steps)))
  newCase$VeryActive <- trunc(newCase$VeryActive + rnorm(nrow(theData),0,sd(newCase$VeryActive)))
  newCase$id <- rep(i,nrow(newCase))
  newCase$grp <- rep(1,nrow(newCase))
  
  if (i == 1)
  {
    pData <- newCase
  }
  else
  {
    pData <- rbind(pData,newCase)
  }
}
for (i in 6:10)
{
  newCase <- theData
  isWeekend <- (newCase$WeekDay == 1) | (newCase$WeekDay == 7)
  newCase$Sleep <- trunc(newCase$Sleep + rnorm(nrow(theData),0,sd(newCase$Sleep))) + 1.5 * sd(newCase$Sleep) * isWeekend
  newCase$Steps <- trunc(newCase$Steps + rnorm(nrow(theData),0,sd(newCase$Steps))) + 1.5 * sd(newCase$Steps) * isWeekend
  newCase$VeryActive <- trunc(newCase$VeryActive + rnorm(nrow(theData),0,sd(newCase$VeryActive))) + 1.5 * sd(newCase$VeryActive) * isWeekend
  newCase$id <- rep(i,nrow(newCase))
  newCase$grp <- rep(1,nrow(newCase))
  
  pData <- rbind(pData,newCase)
}



for (i in 1:10)
{
  fcurv <- getF(nbasis=5,var=pData[which(pData$id == i),]$Steps)
  ffd <- fcurv$ffd
  fbasisobj <- fcurv$basis
  if (i == 1)
  {
    result <-   c(t(ffd[[1]][[1]]),i)
  }
  else
  {
    result <- rbind(result, c(t( ffd[[1]][[1]]),i))
  }
}
result <- cbind(result,c(rep(1,5),rep(2,5)))

for (j in 1:5)
{
  if (j == 1)
  {
    mResult <-t( as.numeric(by(result[,j],result[,7],mean)))
  }
  else 
  { 
    mResult <- rbind(mResult,t( as.numeric(by(result[,j],result[,7],mean))))
  }
}
row.names(mResult) <- c("intercept","sin1","cos1","sin2","cos2")

mResult

for (j in 1:5)
{
  if (j == 1)
  {
    mResult2 <-t( as.numeric(by(result[,j],result[,7],sd)))
  }
  else 
  { 
    mResult2 <- rbind(mResult2,t( as.numeric(by(result[,j],result[,7],sd))))
  }
}
row.names(mResult2) <- c("intercept","sin1","cos1","sin2","cos2")

mResult2
