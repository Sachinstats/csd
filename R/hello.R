# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}
library(tidyverse)
sm.spl<-function(t,delta){
new = data.frame(t,delta)
a1<- new %>% group_by(t, delta) %>%
  tally() %>%
  spread(delta, n)
colnames(a1)=c("Months","No","Yes")
a1[is.na(a1)] <- 0
a1 <- a1 %>% mutate(sno = No/(No + Yes))

n = length(a1$sno)
plot(a1$Months,a1$sno,ylim=c(0,1),pch=1,xlab="Duration"
     ,ylab="1-F(x)",col="black")
months.spl<-smooth.spline(a1$Months,a1$sno,df=7,lambda=0.00002)
months.spl
line= lines(months.spl, col = "green")
months=as.numeric(a1$Months)
a=smooth.spline(a1$Months,a1$sno,df=26.88,lambda=0.00002,cv=TRUE) %>%
  broom::augment()
a1$s1=numeric(n)
for (i in 1:n){
  if (a$.fitted[i]>=1){
    a1$s1[i]=1
  }else{
    a1$s1[i]=a$.fitted[i]
  }
}
a1$F1=numeric(n)
a1$F1=1-a1$s1
a1$f1x=numeric(n)
a1$f1x[1:(n-1)]=diff(a1$F1,1);a1$f1x[n]=1-a1$F1[n]
for ( i in 1:n){
  if (a1$f1x[i]<0){
    a1$f1x[i]=0
  }else{
    a1$f1x[i]=a1$f1x[i]
  }
}

r1 =a1$s1[which(a1$s1 >=0.15& a1$s1 <= 0.35)]
rn1 = length(r1) %% 2
r2 =a1$s1[which(a1$s1 >=0.40 & a1$s1 <= 0.60)]
rn2 = length(r2) %% 2
r3 =a1$s1[which(a1$s1 >=0.65 & a1$s1 <= 0.85)]
rn3 = length(r3) %% 2
if(rn1 == 1 )
{
  rr1 = median(r1)
}else {
  rr1 = r1[length(r1) %/% 2]
}
if(rn2 == 1 )
{
  rr2 = median(r2)
}else {
  rr2 = r2[length(r2) %/% 2]
}
if(rn3 == 1 )
{
  rr3 = median(r3)
}else {
  rr3 = r3[length(r3) %/% 2]
}
q3= a1$Months[which(a1$s1 == rr1)]
q2 = a1$Months[which(a1$s1== rr2)]
q1 = a1$Months[which(a1$s1== rr3)]
a1$fx=numeric(n);a1$fx[n]=1-a1$F1[n]
a1$fx[1:(n-1)]=diff(a1$F1,1)
sum(a1$s1)
a1$ex=as.numeric(a1$Months)*as.numeric(a1$fx)
dd=sum(a1$ex)
out=c(dd,q3,q2,q1)
print(out)
}


###

np_ay<-function(t,delta){
  library(tidyverse)
  new = data.frame(t,delta)
a1<- new %>% group_by(t, delta) %>%
  tally() %>%
  spread(delta, n)
colnames(a1)=c("Months","No","Yes")
a1[is.na(a1)] <- 0
a1 <- a1 %>% mutate(sno = No/(No + Yes))

n = length(a1$sno)
alpha = matrix(nrow=length(a1$No), ncol=length(a1$No))
for (i in 1:length(a1$No)){
  for (j in 1:length(a1$No)){
    alpha[i,j]=sum(a1$No[i:j])
  }
}

beta= matrix(nrow=length(a1$No), ncol=length(a1$No))
for (i in 1:length(a1$No)){
  for (j in 1:length(a1$No)){
    beta[i,j]=sum(a1$No[i:j])+sum(a1$Yes[i:j])
  }
}
A = matrix(nrow=length(a1$No), ncol=length(a1$No))
for (i in 1:length(a1$No)){
  for (j in 1:length(a1$No)){
    A[i,j]=alpha[i,j]/(beta[i,j])
  }
}
pbar = matrix(nrow=(nrow(A)),ncol=1); pbar[1,1] = A[1,1];pbar[nrow(A),1] = A[nrow(A),nrow(A)]
for (i in 2 :(nrow(A)-1)){
  for (j in 1:(ncol(A)-1)){
    p_i=A[c(1:i),c(i:ncol(A))]
    pi=matrix(apply(p_i,1,max),nrow=nrow(p_i),ncol=1)
    pbar[i,]=apply(pi,2,min)
  }
}
a1$pbar=round(pbar,3)

a1$FX=1-a1$pbar
a1$fx=numeric(n);a1$fx[n]=1-a1$FX[n]
a1$fx[1:(n-1)]=diff(a1$FX,1)
dd1=sum(a1$pbar)
a1$ex=as.numeric(a1$Months)*as.numeric(a1$fx)
dd=sum(a1$ex)
###
r1 =a1$pbar[which(a1$pbar >=0.15& a1$pbar <= 0.35)]
rn1 = length(r1) %% 2
r2 =a1$pbar[which(a1$pbar >=0.40 & a1$pbar <= 0.60)]
rn2 = length(r2) %% 2
r3 =a1$pbar[which(a1$pbar >=0.65 & a1$pbar <= 0.85)]
rn3 = length(r3) %% 2
if(rn1 == 1 )
{
  rr1 = median(r1)
}else {
  rr1 = r1[length(r1) %/% 2]
}
if(rn2 == 1 )
{
  rr2 = median(r2)
}else {
  rr2 = r2[length(r2) %/% 2]
}
if(rn3 == 1 )
{
  rr3 = median(r3)
}else {
  rr3 = r3[length(r3) %/% 2]
}
q3 = mean(a1$Months[which(a1$pbar == rr1)])
q2 = mean(a1$Months[which(a1$pbar == rr2)])
q1 = mean(a1$Months[which(a1$pbar == rr3)])
out2=c(dd1,q3,q1,q2)
#p1=plot(a1$Months,a1$sno,ylim=c(0,1),pch=1,xlab="Martial Duration"
 #    ,ylab="1-F(x)",col="black")
#by using ai/Ni method graphical representation
p2=plot(a1$Months,a1$pbar,ylim=c(0,1),type='l',xlab="Duration",main="Duration of First Birth Interval",ylab="1-F(x)",col="green")
A1=data.frame(a1$Months,a1$No,a1$Yes,a1$sno,a1$pbar)
out=list(A1,out2)
my_list <- list(
  table = A1,
  discriptive = out2,
  graph = p2  # Store the graph in the list
)
print(my_list)
}




