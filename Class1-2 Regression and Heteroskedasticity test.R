
rm(list=ls(all=TRUE))

library(data.table)
library(tidyverse)

## Read the BWGHT.csv example into the variable context1
context1 <- fread("BWGHT.csv")
# context1 <- read.csv("BWGHT.csv")

## Data description from BWGHT_labels.txt
# variable name   type    format     label      variable label
# faminc          float   %9.0g                 1988 family income, $1000s
# cigtax          float   %9.0g                 cig. tax in home state, 1988
# cigprice        float   %9.0g                 cig. price in home state, 1988
# bwght           int     %8.0g                 birth weight, ounces
# fatheduc        byte    %8.0g                 father's yrs of educ
# motheduc        byte    %8.0g                 mother's yrs of educ
# parity          byte    %8.0g                 birth order of child
# male            byte    %8.0g                 =1 if male child
# white           byte    %8.0g                 =1 if white
# cigs            byte    %8.0g                 cigs smked per day while preg

## Get summary statistics for all variables
head(context1)
summary(context1)

## Tables of count data
table(context1$male)
table(context1$cigs)
table(context1$male,context1$cigs)

## Basic plotting
plot_basic <- ggplot(context1, aes(x=cigs,y=bwght)) + geom_point() 
plot_basic <- plot_basic + scale_x_continuous(name="Cigarettes smoked during pregnancy per day") + scale_y_continuous(name="Birthweight (oz)")
plot_basic

## Ordinary least-squares the long painful way
y       <- context1$bwght
x       <- context1$cigs
my      <- mean(y)
mx      <- mean(x)
diffy   <- y - my
diffx   <- x - mx
num     <- sum(diffy*diffx)
denom   <- sum(diffx^2)
b1      <- num/denom
b0      <- my - b1*mx
c(b0,b1)

    # bwght = 119.8 + -0.514 cigs

    # Every 1 cigarette smoked by the mother per day during pregancy is associated with a 
    #   0.5 decrease in oz of birthweight.
    # Every pack per day (20 cigs) is associated with a 10 oz decrease in child size at 
    #   birth.
    # Every pack per day is associated with a 0.64 lb decrease in birthweight.

## Convert to lbs and recompute
y         <- y / 16
my        <- mean(y)
diffy     <- y - my
num       <- sum(diffy*diffx)
b1        <- num/denom
20*b1

## Compute OLS using lm
model1    <- lm(bwght~cigs, data=context1)
model1
summary(model1)
coef(model1)

## Plotting OLS
plot_ols   <- plot_basic + geom_line(aes(y=predict(model1)),color="red",size=2)
plot_ols
plot_ols   <- plot_ols + theme(legend.position="none") 
plot_ols #next: redefine everything for easy use later
plot_basic <- ggplot(context1, aes(x=cigs,y=bwght))+geom_point()+theme(legend.position="none")+scale_x_continuous(name="Cigarettes smoked during pregnancy per day")+scale_y_continuous(name="Birthweight (oz)")
plot_basic
plot_ols   <- plot_basic + geom_line(aes(y=predict(model1)),color="red",size=2)
plot_ols

## Multiple least-squares using lm
model2         <- lm(bwght~cigs+faminc, data=context1)
summary(model2)
    # bwght = 119.8 + -0.463 cigs + 0.0928 faminc
    
    # Every 1 cigarette smoked by the mother per day during pregancy is associated with a 
    #   0.5 decrease in oz of birthweight controlling for family income.
model3         <- lm(bwght~cigs+faminc+male+white, data=context1)
model4         <- lm(bwght~cigs+faminc+male+white+motheduc+fatheduc, data=context1)
summary(model3)
summary(model4) #this model drops some data because fatheduc is missing

## BWGHT log transform
tmp            <- log(context1$bwght)
rm("tmp")
context1$lbwght <- log(context1$bwght)
model5         <- lm(lbwght~cigs, data=context1)
model5         <- lm(log(bwght)~cigs, data=context1)
plot_log   <- plot_ols + geom_line(aes(y=exp(predict(model5))),color="blue",size=2)
plot_log

model5         <- lm(log(bwght)~cigs+faminc+male+white+motheduc+fatheduc, data=context1)
summary(model5)
coef(model5)
coef(model5)*100
    # log(bwght) = 4.7115639 + -0.0051484 cigs + 0.0004177 faminc 
    #            + 0.0332363 male + 0.0440468 white + -0.0036268 motheduc
    #            + 0.0033968 fatheduc

    # Every 1 cigarette smoked by the mother per day during pregancy is associated with a 
    #   0.51% decrease in birthweight controlling for family income.

## Quadratic regression
context1$cigsq  <- context1$cigs^2
model6         <- lm(bwght~cigs+cigsq, data=context1)
# model6         <- lm(bwght~cigs+cigs^2, data=context1) #doesn't work (R looks for interactions)
# model6         <- lm(bwght~cigs+I(cig^2), data=context1) #this does work because R treats statements inside of the I() as literal
summary(model6)
plot_quad      <- plot_ols + geom_line(aes(y=predict(model6)),color="blue",size=2)
plot_quad
model7         <- lm(log(bwght)~cigs+cigsq+faminc+male+white+motheduc+fatheduc, data=context1)
summary(model7)

## Subsetting context1
smokectxt      <- context1[cigs > 0]
# smokectxt      <- subset(context1,cigs>0) #data.frame subset for when data.table isn't working
summary(smokectxt)
model8         <- lm(log(bwght)~cigs+male+white, data=smokectxt)
summary(model8)

## Binary variable
context1$smokes         <- as.numeric(context1$cigs>0)
# smokes         <- ifelse(context1$cigs,1,0)
model9        <- lm(log(bwght)~smokes+faminc+male+white+motheduc+fatheduc, data=context1)
summary(model9)

## Adjusting for heteroskedasticity
library(lmtest)
library(sandwich)
coeftest(model9)
coeftest(model9,vcov=vcovHC)

