
rm(list=ls()) #drop all variables

## Package import
######################################
#only run these commands once (ever) OR use the drop-down install
# install.packages('tidyverse',dep=TRUE)
# install.packages('data.table',dep=TRUE)
#run these commands every time you restart R
#  an import statement like this should be at the start of every R file you 
#  create
library(tidyverse)
library(data.table)

## Data import
######################################
#import commands
context1 <- fread('churn.csv')
# context1 <- read.csv('churn.csv')

# Short Description		label
# State				      	state
# Account Length			length
# Area Code			    	code
# Phone					      phone
# Int'l Plan				  intl_plan
# VMail Plan		    	vm_plan
# VMail Message	  		vm_mess
# Day Mins			    	day_mins
# Day Calls			    	day_calls
# Day Charge		  		day_charges
# Eve Mins		    		eve_mins
# Eve Calls			     	eve_calls
# Eve Charge		   		eve_charges
# Night Mins			  	night_mins
# Night Calls			  	night_calls
# Night Charge		  	night_charges
# Intl Mins				    intl_mins
# Intl Calls				  intl_calls
# Intl Charge				  intl_charges
# CustServ Calls			cs_calls
# Churn?					    churn

#summary commands
class(context1) #what type of data do we have?
head(context1) #first 6 rows of the data
summary(context1) #summarize anything in R
colnames(context1) #get the column names of a data set
?summary #get the help files on summary

## Basic data manipulation
######################################
#calling elements in a data.table
context1[1] #calls rows
context1[2]
context1[,1] #calls columns
context1[,2]
context1[,churn] #calls columns using variable names
context1[,cs_calls]
context1$churn #short way to call a column
context1$cs_calls

#discrete variable tabling
table(context1$churn)
table(context1$cs_calls)
table(context1$churn,context1$cs_calls)
table(context1$intl_calls)
table(context1$length)

#vector operations
sum(context1$cs_calls)
mean(context1$cs_calls)
nrow(context1)
sum(context1$cs_calls)/nrow(context1)
median(context1$cs_calls)

## Variable manipulation
######################################
mean(context1$churn) # throws a non-numeric error
tmp <- context1$churn
table(tmp)
tmp
tmp <- tmp=="True."
table(tmp)
tmp <- as.numeric(tmp)
table(tmp)
rm(tmp)

context1$churn <- as.numeric(context1$churn=="True.")
# context1$churn <- ifelse(context1$churn=="True.",1,0)

sum(context1$churn)
mean(context1$churn)


context1$intl_plan  <- as.numeric(context1$intl_plan=="yes")
context1$vm_plan    <- as.numeric(context1$vm_plan=="yes")
context1$bill       <- context1$day_charges+context1$eve_charges+
                        context1$night_charges+context1$intl_charges
summary(context1)

## Basic plotting
######################################
plot(context1$length,context1$cs_calls)
plot_basic <- ggplot(context1, aes(x=length,y=cs_calls)) + geom_point() 
plot_basic
plot_basic <- plot_basic + scale_x_continuous(name="Account length") + 
                scale_y_continuous(name="Customer service calls")
plot_basic

## Group by
######################################
#group by cs_calls
context1[,mean(churn),by=cs_calls]
context1[,.N,by=cs_calls]
context1[,.(.N,mean(churn)),by=cs_calls]
context1[order(cs_calls),.(.N,mean(churn)),by=cs_calls]

#group by others
context1[order(length),.(.N,mean(churn)),by=length]
context1[order(state),.(.N,mean(churn)),by=state]
context1[order(churn),.(.N,mean(length)),by=churn]

#multiple output variables
context1[order(cs_calls),.(.N,mean(length),mean(bill),mean(intl_plan),
                           mean(vm_plan),mean(churn)),by=cs_calls]
outp <- context1[order(cs_calls),.(.N,mean(length),mean(bill),mean(intl_plan),
                                   mean(vm_plan),mean(churn)),by=cs_calls]
colnames(outp) <- c('cs_calls','N','length','bill','intl_plan','vm_plan',
                    'churn')
outp # this fixes the column names
summary(outp)
#get variance instead
 outp <- context1[order(cs_calls),.(.N,var(length),var(bill),var(intl_plan),
                                    var(vm_plan),var(churn)),by=cs_calls]
 colnames(outp) <- c('cs_calls','N','length','bill','intl_plan','vm_plan',
                     'churn')
 outp
rm(outp)

#use cutting to get coarser categories for the length varible
seq(0,250,25)
context1$length_bins <- cut(context1$length,seq(0,250,25),include.lowest=TRUE)
table(context1$length_bins)
outp <- context1[order(length_bins),.(.N,mean(bill),mean(cs_calls),
                                      mean(intl_plan),mean(vm_plan),
                                      mean(churn)),by=length_bins]
colnames(outp) <- c('length_bins','N','bill','cs_calls','intl_plan','vm_plan',
                    'churn')
outp
rm(outp)

## Correlations
######################################
cor(context1$length,context1$churn)
cor(context1$cs_calls,context1$churn)
cor(context1$bill,context1$churn)
attach(context1)
tmp <- cbind(length,bill,intl_plan,vm_plan,churn,cs_calls)
detach(context1)
cor(tmp)
rm(tmp)

