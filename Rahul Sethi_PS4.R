# Problem Set 4
# Rahul Sethi

rm(list=ls())

setwd("C:/Users/Rahul/Desktop/UTD/Course Selection/BUAN6356/HW4")

library(RSQLite)
library(tidyverse)
library(grid)
library(plm)
library(margins)
library(Greg)
library(data.table)

# Question 4.1

# The best model is Model 5:
#     log(price) ~ sqrft + colonial + lotsize + I(lotsize^2)
# The AIC and BIC values for Models 1 thorugh 5 are:
#           AIC     BIC
#     1 -38.05228 -23.18826
#     2 -47.61833 -27.79964
#     3 -47.19900 -24.90297
#     4 -49.03084 -31.68948
#     5 -51.01075 -36.14673

con <- dbConnect(SQLite(), "wooldridge.db")
hprice1 <- dbReadTable(con,"HPRICE1")
dbReadTable(con,"HPRICE1_labels")
dbDisconnect(con)

hist(hprice1$price)
hist(hprice1$lprice)
hprice1 <- hprice1[,c(1,2,8,3,4,5,6,7,9,10,11)]
hprice1$index <- NULL

for(i in 3:ncol(hprice1)){
    pprplot <- qplot(hprice1$price, hprice1[,i], data=hprice1, geom=c("point"), ylab = colnames(hprice1)[i])
    lprplot <- qplot(hprice1$lprice, hprice1[,i], data=hprice1, geom=c("point"), ylab = colnames(hprice1)[i])
    pushViewport(viewport(layout = grid.layout(1, 2)))
    print(pprplot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(lprplot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
    readline(prompt = "Press Enter...")
}
ggplot(hprice1, aes(x=as.factor(colonial), y=lprice)) + geom_boxplot()
ggplot(subset(hprice1, lotsize <75000), aes(x=lprice, y=lotsize)) + geom_point() + geom_smooth()

housemodels <- list(1)
housemodels[[1]] <- lm(lprice~bdrms + sqrft + colonial + lotsize, data=hprice1)
housemodels[[2]] <- lm(lprice~bdrms + sqrft + I(sqrft^2) + colonial + lotsize + I(lotsize^2), data=hprice1)
housemodels[[3]] <- lm(lprice~bdrms + I(bdrms^2) + sqrft + I(sqrft^2) + colonial + lotsize + I(lotsize^2), data=hprice1)
housemodels[[4]] <- lm(lprice~sqrft + I(sqrft^2) + colonial + lotsize + I(lotsize^2), data=hprice1)
housemodels[[5]] <- lm(lprice~sqrft + colonial + lotsize + I(lotsize^2), data=hprice1)

for(i in 1:length(housemodels)){
    print(c(i,AIC(housemodels[[i]]),BIC(housemodels[[i]])))
}


# Question 4.2

# The best model is Model 10:
#     colgpa ~ sat^2 + tothrs + hsize + log(hsrank)^2 + log(hsperc) + athlete + black + female
# 
# The AIC and BIC Values for models 1 through 8 are:
#     Model AIC      BIC
#     1     6707.952 6777.557
#     2     6759.675 6829.280
#     3     6707.018 6763.967
#     4     6707.018 6763.967
#     5     6707.188 6757.810
#     6     7047.172 7085.138
#     7     6708.787 6753.081
#     8     6705.201 6749.495
#     9     6544.170 6613.775
#     10    6542.187 6605.465

con <- dbConnect(SQLite(), "wooldridge.db")
gpa2 <- dbReadTable(con,"GPA2")
dbReadTable(con,"GPA2_labels")
dbDisconnect(con)

hist(gpa2$colgpa)
gpa2 <- gpa2[,c(1,4,2,3,6,7,8,9,13,5,10,11,12)]
gpa2$index <- NULL

for(i in 2:8){
    gpln <- qplot(gpa2$colgpa,gpa2[,i], data=gpa2, geom=c("point","smooth"), xlab="gpa", ylab = colnames(gpa2)[i])
    gplln <- qplot(log(gpa2$colgpa),gpa2[,i], data=gpa2, geom=c("point","smooth"), xlab="log(gpa)", ylab = colnames(gpa2)[i])
    gplg <- qplot(gpa2$colgpa,log(gpa2[,i]), data=gpa2, geom=c("point","smooth"), xlab="gpa", ylab = paste("log ",colnames(gpa2)[i]))
    gpllg <- qplot(log(gpa2$colgpa),log(gpa2[,i]), data=gpa2, geom=c("point","smooth"), xlab="log(gpa)", ylab = paste("log ",colnames(gpa2)[i]))
    
    pushViewport(viewport(layout = grid.layout(2, 2)))
    print(gpln, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(gplln, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
    print(gplg, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
    print(gpllg, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
    readline(prompt = "Press Enter...")
}

for(i in 9:12){
    print(ggplot(data=gpa2, aes(x=as.factor(gpa2[,i]), y=colgpa)) + geom_boxplot() + xlab(colnames(gpa2)[i]))
    readline(prompt = "Press Enter...")
}

table(gpa2$white + gpa2$black)
table(gpa2$white == gpa2$black)

gpamodels <- list()
gpamodels[[1]]<-lm(colgpa ~ sat + I(sat^2) + tothrs + hsize + I(hsize^2) + log(hsrank) + I(log(hsrank)^2) + log(hsperc) + I(log(hsperc)^2), data=gpa2)
gpamodels[[2]]<-lm(colgpa ~ sat + I(sat^2) + exp(tothrs) + hsize + I(hsize^2) + log(hsrank) + I(log(hsrank)^2) + log(hsperc) + I(log(hsperc)^2), data=gpa2)
gpamodels[[3]]<-lm(colgpa ~ sat + I(sat^2) + tothrs + hsize + I(hsize^2) + log(hsrank) + I(log(hsrank)^2) + log(hsperc), data=gpa2)
gpamodels[[4]]<-lm(colgpa ~ sat + I(sat^2) + tothrs + hsize + I(hsize^2) + I(log(hsrank)^2) + log(hsperc), data=gpa2)
gpamodels[[5]]<-lm(colgpa ~ sat + I(sat^2) + tothrs + hsize + I(log(hsrank)^2) + log(hsperc), data=gpa2)
gpamodels[[6]]<-lm(colgpa ~ tothrs + hsize + I(log(hsrank)^2) + log(hsperc), data=gpa2)
gpamodels[[7]]<-lm(colgpa ~ sat + tothrs + hsize + I(log(hsrank)^2) + log(hsperc), data=gpa2)
gpamodels[[8]]<-lm(colgpa ~ I(sat^2) + tothrs + hsize + I(log(hsrank)^2) + log(hsperc ), data=gpa2)
gpamodels[[9]]<-lm(colgpa ~ I(sat^2) + tothrs + hsize + I(log(hsrank)^2) + log(hsperc ) + athlete + female + black + white, data=gpa2)
gpamodels[[10]]<-lm(colgpa ~ I(sat^2) + tothrs + hsize + I(log(hsrank)^2) + log(hsperc ) + athlete + female + black, data=gpa2)

anova(gpamodels[[5]],gpamodels[[6]])

summary(gpamodels[[8]])

for(i in 1:length(gpamodels)){
    print(c(i,AIC(gpamodels[[i]]),BIC(gpamodels[[i]])))
}


# Question 4.3



con <- dbConnect(SQLite(), "wooldridge.db")
mlb1 <- dbReadTable(con,"MLB1")
dbReadTable(con,"MLB1_labels")
dbDisconnect(con)

mlb1_t <- mlb1[complete.cases(mlb1),]
mlb1_t$index <- NULL
mlb1_t$lsalary <- NULL

library(leaps)
library(MASS)
mlbLM <- regsubsets(salary ~ ., data=mlb1_t, method = "backward")
mlbLM1 <- regsubsets(salary ~ ., data=mlb1_t, method = "forward")

summary(mlbLM1)

finmlbLM <- lm(salary ~ allstar + rbisyr + yrsallst + runs, data = mlb1_t)
summary(finmlbLM)
BIC(finmlbLM)



# Question 4.4

# 4.4.i: The result in the standard form are[SEs in {}]:
#       log(rent) = -0.5688065{0.5348806} + 0.2622267*y90{0.0347632} + 0.0406863*log(pop){0.0225154} + 0.5714461*log(avginc){0.5714461} + 0.0050436*pctstu{0.0050436}
# The cofficient on y90 shows me that the rent has increased by 26.2% from 1980 to 1990.
# The cofficient on pctstu shows me that a 10 unit increase in pctstu(10% increase in students), the rent increases by 5%.

# 4.4.ii: Not sure of this yet

# 4.4.iii: By first differencing, the cofficient of pctstu becomes much larger. Now, for a 10% increase in student population, the rent increases by 18.7% - much higher from our pooled OLS estimate. The relaive size of the student population most certainly seems to affec rental prices.

# 4.4.iv: Using the fixed effects model, I get an estimate of 0.0112 or, for a 10% increase in student population, the rent increases by 11.2%. The estimate is different from part iii, but significant - both statistically and financially.

library(plm)
con <- dbConnect(SQLite(), "wooldridge.db")
rental <- dbReadTable(con,"RENTAL")
dbReadTable(con,"RENTAL_labels")
dbDisconnect(con)
rental <- plm.data(rental, indexes = c("city","year"))

# Estimating Pooled OLS Model
summary(plm(log(rent) ~ y90 + log(pop) + log(avginc) + pctstu, model="pooling", data=rental))
# Estimating first-difference model
summary(plm(log(rent) ~ y90 + log(pop) + log(avginc) + pctstu, model="fd", data=rental))
# Estimating fixed effects model
summary(plm(log(rent) ~ y90 + log(pop) + log(avginc) + pctstu, model="within", effects="twoway", data=rental))



# Question 4.5

# 4.5.i: I would expect Beta1 to be negative. Historically, that is the argument that has been used for the death penalty - that executions in heinous crimes deter potential offendors. For Beta2, I would expect a positive sign. High unemployment usually leads to higher crime-rates.

# 4.5.ii: In the pooled model, there is some evidence for a detrimental effect.

# 4.5.iii: It is still negative in magniture and barely significant at a 5% level.

# 4.5.iv: Done. Estimates don't change.

# 4.5.v: The highest in for Texas at 34 and the second place is for Virginia at 11.

# 4.5.vi: Although negative, the detrimental effect ceases to be significant at a 5% level - both with and without Hetroskedasticity correction.

# 4.5.vii: With HC correction, the cofficient is not significant at a 5% level.


con <- dbConnect(SQLite(), "wooldridge.db")
murder <- dbReadTable(con,"MURDER")
dbReadTable(con,"MURDER_labels")
dbDisconnect(con)

murder <- pdata.frame(murder, index = c("id","year"))

murder1 <- subset(murder, year %in% c(90,93))

# Pooled OLS model
summary(plm(mrdrte ~ exec + unem + state + as.factor(year), data=murder1, model="pooling"))

# Fixed Effects model with first-differencing
#summary(plm(cmrdrte ~ cexec + cunem, data=murder1, model="within"))
summary(plm(mrdrte ~ exec + unem, data=murder1, model="fd"))
summary(plm(mrdrte ~ exec + unem, data=murder1, model="within",effects="twoways"))

summary(plm(mrdrte ~ exec + unem, data=murder1, model="within",effects="twoways"),vcovHC)

subset(murder[order(-murder$exec),], year==93)[c(1,2),c("state","exec")]

#Removing Texas and performing analysis

murder2 <- subset(murder1,state != "TX")

summary(plm(mrdrte ~ exec + unem, data=murder2, model="fd"))
summary(plm(mrdrte ~ exec + unem, data=murder2, model="fd"),vcovHC)

#Using all the data
summary(plm(mrdrte ~ exec + unem, data=murder, model="within", effect = "twoways"))
summary(plm(mrdrte ~ exec + unem, data=murder, model="within", effect = "twoways"), vcovHC)


# Question 4.6

# 4.6.i: The estimated increase in fare would be 3.6%

# 4.6.ii: The interval estimate from the OLS model is [0.3011849,0.4190557]. After HC correction, the new interval is [0.2454754,0.4747652].

# 4.6.iii: The values should start becoming positive at dist = 79.50877, a value which is smaller than the smallest value of dist in the data. Effectively, for all values of dist in the data-set, log(fare) should increase, controling for all other factors.

# 4.6.iv: The new estimate is 0.103051.

# 4.6.v: One would be the geography of the route (eg: South only, South-East, etc) and the other is vicinity to tourist attractions (eg: DFW to LAX is a route if I want to go to Disneyworld). Yes these might be correlated with concen as only some airlines serve specific routes.

# 4.6.vi: From economic theory, I should think increase in concen would increase fare on a route because of monopoly. If a single airline captures a route, they would want to increase fares because competition is no longer a threat. I would place my estimate at that of the last model i.e. 0.103051.


con <- dbConnect(SQLite(), "wooldridge.db")
airfare <- dbReadTable(con,"AIRFARE")
dbReadTable(con,"AIRFARE_labels")
dbDisconnect(con)

airfare <- pdata.frame(airfare, index=c("id","year"))

summary(plm(lfare ~ as.factor(year) + concen + ldist + I(ldist^2), data = airfare, model="pooling"))
summary(plm(lfare ~ as.factor(year) + concen + ldist + I(ldist^2), data = airfare, model="pooling"),vcovHC)

c(0.3601203-1.96*0.0300691 ,0.3601203+1.96*0.0300691)
c(0.3601203 - 1.96*0.0584923, 0.3601203 + 1.96*0.0584923)

exp(-(-0.9016002)/(2*0.1030196))
summary(airfare)

# Just testing my claim about monotonic increase
airmodel <- plm(lfare ~ as.factor(year) + id + concen + ldist + I(ldist^2), data = airfare, model="pooling")
testair <- data.table(year=airfare$year,concen = mean(airfare$concen),ldist = airfare$ldist)
testair$predictions <- predict(airmodel,testair)
ggplot(testair, aes(x=ldist, y=predictions)) + geom_path(aes(color=year))
rm(airmodel)
rm(testair)


# Fixed Effects model
summary(plm(lfare ~ concen + ldist + I(ldist^2), data = airfare, model="within", effects="twoways"))


# Question 4.7

# 4.7.i: The calculated percentages for both logit and linear models are 0.7077922 0.9083879 for non-white and white people respectively. This looks like a significant difference, however, I cannot say there is discrimination by looking only at the race.

# 4.7.ii: Even now, all things considered, the respective probabilities are 0.8280339 0.9248066. This would point towards discrimination. Also, the variable white is significant in the model summary.

con <- dbConnect(SQLite(), "wooldridge.db")
loan <- dbReadTable(con,"LOANAPP")
dbReadTable(con,"LOANAPP_labels")
dbDisconnect(con)

logapp <- glm(approve ~ white, data=loan, family = "binomial")
linapp <- lm(approve ~ white, data=loan)
predict(logapp, data.frame(white=c(0,1)),type="response")
predict(linapp, data.frame(white=c(0,1)))

logapp2 <- glm(approve ~ white + hrat + obrat + loanprc + unem + male + married + dep + sch + cosign + chist + pubrec + mortlat1 + mortlat2 + vr,data=loan, family = "binomial")

a<- data.frame(white=c(0,1),hrat=mean(loan$hrat), obrat=mean(loan$obrat), loanprc=mean(loan$loanprc), unem=mean(loan$unem), male=mean(loan$male, na.rm = TRUE), married=mean(loan$married, na.rm = TRUE), dep=mean(loan$dep, na.rm = TRUE), sch=mean(loan$sch), cosign=mean(loan$cosign), chist=mean(loan$chist), pubrec=mean(loan$pubrec), mortlat1=mean(loan$mortlat1), mortlat2=mean(loan$mortlat2), vr=mean(loan$vr))

predict(logapp2, a, type="response")

summary(logapp2)


# Question 4.8

