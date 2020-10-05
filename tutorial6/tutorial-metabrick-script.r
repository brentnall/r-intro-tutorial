## METABRIC clinical data, exploratory analysis
##

## 1. Load data
## Data from https://www.cbioportal.org/study/clinicalData?id=brca_metabric
mydta = read.delim("brca_metabric_clinical_data.tsv", sep="\t")

## 2. Show first few rows, list column names

head(mydta)

View(mydta)

## 3. Summary statistics

summary(mydta)

## 4. Some univariate plots

## 4.1 Age at diagnosis

par(mfrow=c(2,1))

hist(mydta$Age.at.Diagnosis, xlab="Age", main="Age Histogram")

boxplot(mydta$Age.at.Diagnosis, xlab="Age", main="Age Boxplot", horizontal=TRUE)

## 4.2 Nodes

par(mfrow=c(2,1))

hist(mydta$Lymph.nodes.examined.positive, breaks=seq(0,max(mydta$Lymph.nodes.examined.positive, na.rm=TRUE), by=1), col=1, main="")

boxplot(mydta$Lymph.nodes.examined.positive, xlab="Nodes", main="Nodes Boxplot", horizontal=TRUE)

table(mydta$Lymph.nodes.examined.positive)

## 4.3 Tumour size

par(mfrow=c(2,1))

hist(mydta$Tumor.Size, xlab="Tumour size", main="Tumour size Histogram")

boxplot(mydta$Tumor.Size, xlab="Tumour size", main="Tumour size Boxplot", horizontal=TRUE)

## 4.4 Tumour size vs Lymph nodes

plot(mydta$Tumor.Size, mydta$Lymph.nodes.examined.positive, log="x")

plot(mydta$Lymph.nodes.examined.positive,mydta$Tumor.Size, log="y")

mydta$LNpos = mydta$Lymph.nodes.examined.positive >0

tapply(mydta$Tumor.Size, mydta$LNpos, summary)

sizeneg = subset(mydta$Tumor.Size, mydta$LNpos)

sizepos = subset(mydta$Tumor.Size, !mydta$LNpos)

boxplot(sizeneg, sizepos)
par(mfrow=c(2,1))
hist(sizeneg, xlim=c(0,80), breaks=seq(0,200,by=5), col=1)
hist(sizepos, xlim=c(0,80), breaks=seq(0,200,by=5), col=1)

t.test(sizeneg, sizepos)

wilcox.test(sizeneg, sizepos)

## binary

npos = c(sum(sizeneg>20, na.rm=TRUE), sum(sizepos>20, na.rm=TRUE))

ntot = c(sum(!is.na(sizeneg)), sum(!is.na(sizepos)))

## number size >20mm
npos

## number with size available
ntot

## proportion

npos / ntot

## same as
mean(sizeneg>20, na.rm=TRUE)
mean(sizepos>20, na.rm=TRUE)


##test equality two proportions
prop.test(npos, ntot)

