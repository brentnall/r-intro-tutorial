###################################
## CANM937
## RStudio tutorial 1
## R Script - analysis commands
## v-060920
###################################

## 1. Load data

mydta = read.csv("chirps.csv")

## 2. Look at data loaded

head(mydta)

View(mydta)

## 3. Do a scatter plot

plot(mydta)

## 4. Calculate Pearson correlation

cor.test(mydta[,1], mydta[,2])

## 5. Fit a linear regression

mylm = lm(Temperature~Chirps.Minute, mydta)

#6. Should results from linear regression fit

summary(mylm)

plot(mydta)

lines(mydta[,1], predict(mylm), col=2)

plot(mylm)
