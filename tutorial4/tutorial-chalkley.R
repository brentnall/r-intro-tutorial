## -----------------------------------------------------------------------------
##load avb6 data
mydta=read.csv("181008-classdta.csv")
colnames(mydta)


## -----------------------------------------------------------------------------
## look at data first few rows using head() function
head(mydta)


## -----------------------------------------------------------------------------
table(mydta$B6)


## -----------------------------------------------------------------------------
table(mydta$B6t)


## -----------------------------------------------------------------------------
summary(mydta$CHALKLEY)


## -----------------------------------------------------------------------------
#Number in each GRADE category
table(mydta$GRADE)


## -----------------------------------------------------------------------------
## Chalkley by avb6 positivity
png(file="charts/box1.png")
boxplot(split(mydta$CHALKLEY, mydta$B6), xlab="avb6 positivity", ylab="Chalkley count")
dev.off()

## Chalkley by avb6 trend
png(file="charts/box2.png")
boxplot(split(mydta$CHALKLEY, mydta$B6t), names=c("Neg", "Het", "Pos"), ylab="Chalkley count", xlab="avb6 category")
dev.off()


## -----------------------------------------------------------------------------
tapply(mydta$CHALKLEY, mydta$B6, summary)


## -----------------------------------------------------------------------------
tapply(mydta$CHALKLEY, mydta$B6t, summary)


## -----------------------------------------------------------------------------
##classical t-test - assuming constant variance, t-distribution reference
myttest=t.test(mydta$CHALKLEY[mydta$B6==0], mydta$CHALKLEY[mydta$B6==1], var.equal=TRUE)
myttest


## -----------------------------------------------------------------------------
##t-test that allows different variance between groups (Welch t-test)
t.test(mydta$CHALKLEY[mydta$B6==0], mydta$CHALKLEY[mydta$B6==1], var.equal=FALSE)


## -----------------------------------------------------------------------------
wilcox.test(mydta$CHALKLEY[mydta$B6==0], mydta$CHALKLEY[mydta$B6==1])


## -----------------------------------------------------------------------------
crosstab=table(mydta$GRADE==2,mydta$B6==1)
crosstab
ngrade=table(mydta$GRADE==2)
npos=crosstab[,2]
prop.test(npos, ngrade)

