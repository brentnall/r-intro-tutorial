## -----------------------------------------------------------------------------
mydta<-read.csv("kidney.txt", sep=" ") #columns separated by whitespace, not commar
head(mydta)


## -----------------------------------------------------------------------------
png(file="plot-kidney.png")
plot(mydta$age, mydta$tot, xlab="Age (y)", ylab="Measure of kidney function")
dev.off()


## -----------------------------------------------------------------------------
myreg<-lm(tot~age, mydta)
summary(myreg)
round(confint(myreg)*10,2)


## -----------------------------------------------------------------------------
round(predict(myreg, newdata=data.frame(age=70)),1)


## -----------------------------------------------------------------------------
png('plot-kidney-reg1.png')
plot(mydta$age, mydta$tot, xlab="Age (y)", ylab="Measure of kidney function")
grid()
abline(myreg, col="red")
newx <- seq(1, 90, by=0.2)
conf_interval <- predict(myreg, newdata=data.frame(age=newx), interval="confidence",level = 0.95)
lines(newx, conf_interval[,2], col="blue", lty=2)
lines(newx, conf_interval[,3], col="blue", lty=2)
dev.off()


## -----------------------------------------------------------------------------
## Standard R diagnostic plots for linear regression
png("plot-kidney-reg2.png")
plot(myreg,1)
dev.off()

png("plot-kidney-reg3.png")
plot(myreg,2)
dev.off()


## -----------------------------------------------------------------------------
cor.test(mydta$age, mydta$tot)


## -----------------------------------------------------------------------------
cor.test(mydta$age, mydta$tot, method="spearman")

