###  exp 2  #####


empid=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
age=c(30,37,45,32,50,60,35,32,34,43,32,30,43,50,60)
gender=c(0,1,0,1,1,1,0,0,1,0,0,1,1,0,0)
status=c(1,1,2,2,1,1,1,2,2,1,2,1,2,1,2)
empinfo=data.frame(empid,age,gender,status)
empinfo$gender=factor(empinfo$gender,labels=c("male","female"))
empinfo$status=factor(empinfo$status,labels=c("staff","faculty"))
empinfo
summary(empinfo)
male=subset(empinfo,empinfo$gender=="male")
summary(male)
female=subset(empinfo, empinfo$gender=='female')
summary(female)
summary(age)
table1=table(empinfo$gender)
table1
table2=table(empinfo$status)
table2
table3=table(empinfo$gender, empinfo$status)
table3
plot(empinfo$age,type="l",main="Age of employees",xlab="empid",ylab="age in years",col="blue")
pie(table1)
barplot(table3,beside=T,xlim=c(1,15),ylim=c(0,5),col=c("blue", "red")) 
legend("topright",legend=rownames(table3),fill=c('blue','red'),bty="n")
boxplot(empinfo$age~empinfo$status,col=c('red','blue'))



###########    EXP 3 prob 1 	###########

data=cars
v1=var(data$speed)
v2=var(data$dist)
covariance=cov(data$speed,data$dist)
corr=cor(data$speed,data$dist)
cor.test(data$speed,data$dist)
cor.test(data$speed,data$dist,method="pearson")
cor.test(data$speed,data$dist,method="spearman")
plot(data$speed,data$dist)
regression1=lm(data$speed~data$dist)
abline(regression1)
regression2=lm(data$dist~data$speed)

########## 	EXP3 PROB2 ##########
##########	BMI PROBLEM #########

data=cars
v1=var(data$speed)
v2=var(data$dist)
covariance=cov(data$speed,data$dist)
corr=cor(data$speed,data$dist)
cor.test(data$speed,data$dist)
cor.test(data$speed,data$dist,method="pearson")
cor.test(data$speed,data$dist,method="spearman")
plot(data$speed,data$dist)
regression1=lm(data$speed~data$dist)
abline(regression1)
regression2=lm(data$dist~data$speed)


########### exp 4 prob 1 ##########

##### Problem 1: The sale of a Product in lakhs of rupees (Y) is expected to be influenced by two variables namely the advertising expenditure X1 (in'OOORs) and the number of sales persons (X2) in a region. Sample data on 8 Regions of a state has given the following results



Y=c(110,80,70,120,150,90,70,120)
X1=c(30,40,20,50,60,40,20,60)
X2=c(11,10,7,15,19,12,8,14)
RegModel=lm(Y~X1+X2)
library(scatterplot3d)
scatterplot3d(Y,X1,X2)

#################
#################	EXP 4 PROB 2	#######
#Problem 2

data=mtcars
X=mtcars$mpg
Y=mtcars$disp
Z=mtcars$hp
RegModel<- lm(Z~X+Y)
library(scatterplot3d)
graph=scatterplot3d(X,Y,Z)
graph$plane3d(RegModel)


#############
#########		EXP 5 ########
###Binomial distribution

data=mtcars
X=mtcars$mpg
Y=mtcars$disp
Z=mtcars$hp
RegModel<- lm(Z~X+Y)
library(scatterplot3d)
graph=scatterplot3d(X,Y,Z)
graph$plane3d(RegModel)


################3
########		EXP 6 prob  2  ###############
#####Normal distribution,

x=seq(0,40)
y=dnorm(x,mean=20,sd=5)
plot(x,y,type='l')
p1=pnorm(15,mean=20,sd=5)
x2=seq(0,15)
y2=dnorm(x2,mean=20,sd=5)
polygon(c(0,x2,15),c(0,y2,0),col='yellow')
p2=pnorm(40,mean=20,sd=5)-pnorm(25,mean=20,sd=5)
x1=seq(25,40)
y1=dnorm(x1,mean=20,sd=5)
polygon(c(25,x1,40),c(0,y1,0),col='red')
p3=pnorm(25,mean=20,sd=5)-pnorm(15,mean=20,sd=5)
x3=seq(15,25)
y3=dnorm(x3,mean=20,sd=5)
polygon(c(15,x3,25),c(0,y3,0),col='green')
data.frame(p1,p2,p3)

#####################
########## POISSIONS DISTRUBUTION


n=4
p=0.5
dbinom(2,n,p)
sum(dbinom(2:4,n,p))
sum(dbinom(0:2,n,p))
x=0:n 
px=dbinom(x,n,p) 
Ex=weighted.mean(x,px)
Varx=weighted.mean(x*x,px)-(weighted.mean(x ,px))^2
plot(x,px,type="h",xlab="values of x",ylab="Probability distribution of x",main="Binomial distribution")


############
########  	EXP 7 PROB 1  ###########
#######		Test of significance of the difference between sample mean and population mean		########
xbar=14.6
mu0=15.4
sigma=2.5
n=35
z=(xbar-mu0)/(sigma/sqrt(n))
alpha=0.05
zhalfalpha=qnorm(1-(alpha/2))
c(-zhalfalpha,zhalfalpha)
pval=2*pnorm(z)
if(pval>alpha){print("Accept Null hypothesis")} else{print("Reject Null hypothesis")}

######
####################	 EXP 7 PROB 2 ############
########## 	Test of significance of the difference between sample proportion and population proportion 

n=640
Sprop=63/n
Pprop=0.1726
q=1-Pprop
z=(Sprop-Pprop)/sqrt(Pprop*q/n)
E=qnorm(.975)
c(-E,E)
Sprop+c(-E,E)*sqrt(Pprop*(1-Pprop)/n)
if(z>-E && z<E){print("Hospital is not efficient")} else{print("Hospital is efficient")}


