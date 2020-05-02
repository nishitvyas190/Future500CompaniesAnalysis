library(ggplot2)
getwd()
setwd("D:\\R PROGRAMMING - ADVANCED ANALYTICS IN R FOR DATA SCIENCE\\02 Data Preparation")
fin<- read.csv("Future-500.csv",na.strings = "")
head(fin,24)

fin$ID<-factor(fin$ID)
fin$Inception<-factor(fin$Inception)
str(fin)
fin$Expenses<-gsub(" Dollars","",fin$Expenses)
fin$Expenses<-gsub(",","",fin$Expenses)
fin$Revenue<-gsub("\\$","",fin$Revenue)
fin$Revenue<-gsub(",","",fin$Revenue)
head(fin)
str(fin)
fin$Growth<-gsub("%","",fin$Growth)


fin$Growth<-as.numeric(fin$Growth)
fin$Revenue<-as.numeric(fin$Revenue)
fin$Expenses<-as.numeric(fin$Expenses)
str(fin)
summary(fin)

fin[!complete.cases(fin),]
fin_backup<- fin
fin<-fin[!is.na(fin$Industry),]


fin[(is.na(fin$State)&fin$City=="New York"),"State"]
fin[(is.na(fin$State) & fin$City=="San Francisco"),"State"]
fin[is.na(fin$State)&fin$City=="San Francisco","State"]<-"CA"
fin[is.na(fin$State),]


#applying median 
fin[is.na(fin$Employees) & fin$Industry=="Retail",]
med_retail_emp<-median(fin[fin$Industry=="Retail","Employees"],na.rm = TRUE)
fin[is.na(fin$Employees) & fin$Industry=="Retail","Employees"]<- med_retail_emp
#check
fin[3,]

fin[is.na(fin$Employees),]
med_finserv_emp<-median(fin[fin$Industry=="Financial Services","Employees"],na.rm = TRUE)
fin[is.na(fin$Employees),"Employees"]<- med_finserv_emp
fin[330,]
rownames(fin)<-NULL
head(fin,20)



med_growth_constr<-median(fin[fin$Industry=="Construction","Growth"],na.rm = TRUE)
med_growth_constr
fin[is.na(fin$Growth),"Growth"]<-med_growth_constr
fin[8,]


fin[!complete.cases(fin),]
med_rev_contr<-median(fin[fin$Industry=="Construction","Revenue"],na.rm = TRUE)
med_rev_contr
fin[is.na(fin$Revenue),"Revenue"]<-med_rev_contr


med_exp_contr<-median(fin[fin$Industry=="Construction","Expenses"],na.rm = TRUE)
med_exp_contr
fin[is.na(fin$Expenses)&fin$Industry=="Construction","Expenses"]<- med_exp_contr

#add_sub

#Expenses=Revenue-Profit
#Profit=Revenue-Expenses

fin[is.na(fin$Profit),"Profit"]=fin[is.na(fin$Profit),"Revenue"]-fin[is.na(fin$Profit),"Expenses"]
fin[!complete.cases(fin),]


fin[is.na(fin$Expenses),"Expenses"]=fin[is.na(fin$Expenses),"Revenue"]-fin[is.na(fin$Expenses),"Profit"]
rownames(fin)<-NULL

#visulisation
p<-ggplot(data=fin,aes(x=Revenue,y=Expenses,size=Profit))
p+geom_point(aes(colour=Industry))

d<- ggplot(data=fin,aes(x=Revenue,y=Expenses,colour=Industry))
d+ geom_point() + geom_smooth(fill=NA)

e<- ggplot(data=fin, aes(x=Industry,y=Growth,colour=Industry))
e+geom_jitter()+ geom_boxplot(size=1,alpha=0.5,outlier.colour = NA)

