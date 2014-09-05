#Jayavardhan Daripineni

#### Question 1 ####
Vect <- c(2,3,1,48,2,64,5,67,76,8,2,8,3,89,6,8,13,25,12,41)

#### Question 2 ####
v<-as.character(Vect)

#### Question 3 ####
vf<-as.factor(Vect)

#### Question 4 ####
nlevels(vf)

#Result: [1] 15

#### Question 5 ####
v <- 3 * Vect * Vect - 4 * Vect + 1

#### Question 6 ####
X = matrix(
    c(1,5,8,1,4,9,1,6,4,1,2,7,1,3,4,1,2,9,1,7,6,1,8,4),
    nrow=8,
    ncol=3,
    byrow=TRUE)

Y = matrix(
    c(45.2,46.9,31.0,35.3,25.0,43.1,41.0,35.1),
    nrow=8,
    ncol=1,
    byrow=TRUE)

B = solve(t(X) %*% X) %*% t(X) %*% Y

#B is calculated to the values shown in the question, rounded on two decimals.

#### Question 7 ####
namedList = list(temperatures=c(25,39,55,77,67), days=c("mon", "tue", "wed")) 

#### Question 8 ####
#Create a date vector (DoB) and use it while creating the data frame DF2
DoB=as.Date(c("2000-03-22","1990-08-05","1988-12-03","2001-09-30","1999-10-14","1986-02-28","1966-11-11","2012-12-12","1978-04-26","1967-04-21"))
DF2=data.frame(name=c('Adam','Bob','Cindy','Doug','Emmi','Fitch','Greg','Horton','Igor','Jay'), 
               AgeGroup=factor(c("<10","11-30",">30","<10","11-30",">30","<10","11-30",">30","<10")), 
               Age=c(12,55,11,65,22,23,54,17,27,34), 
               DoB)
#Convert name column exclusively to character as it is created as factor.
DF2$name <- as.character(DF2$name)

#### Question 9 ####
#To add a row with a factor value not in existing values, we should convert the factor column to character first
DF2$AgeGroup<-as.character(DF2$AgeGroup)
#Then add the row after conversion above
DF2<-rbind(DF2,c("Kim","60+",2,"2012-05-09"))
#Convert the column back to factor
DF2$AgeGroup<-as.factor(DF2$AgeGroup)

#### Question 10 ####
#Full file path is not needed if the file is in working directory.
Temperatures<- read.csv("temperatures.csv", sep=",", header = TRUE, stringsAsFactors = FALSE)

#### Question 11 ####
Measurements<- read.delim("C:/Users/jdaripin/Desktop/measurements.txt", sep="\t", header = TRUE, stringsAsFactors = FALSE)

#### Question 12 ####
DataSet<-read.delim("https://raw.githubusercontent.com/jayavardhand/MSDA-IS607-Data-Acquisition-and-Management/master/Assignment-1/Test-Pipe-Delimited.txt",sep="|",header=T)

#### Question 13 ####
factorial=1
for (i in c(1:12)) {
  factorial<-factorial * i
}
#factorial value will be 479001600

#### Question 14 ####
Principal=1500
for (i in c(1:(6*12))) {
  Principal<-Principal + Principal*0.0324
}
Principal<-round(Principal,digits=2)
#Principal value is 14898.65

#### Question 15 ####
v<-c(2,3,1,48,2,64,5,67,76,8,2,8,3,89,6,8,13,25,12,41)
sum(v[seq(0, length(v), 3)])
#This will be sum(1 64 76 8 6 25) and equals to 180

#### Question 16 ####
x<-2
GPSum<-0
for (i in 1:10){
  GPSum<-GPSum+x^i
}
#Value of GPSum as seen in work space is 2046

#### Question 17 ####
x<-2
GPSum<-0
i=1
while (i<=10){
  GPSum = GPSum+x^i
  i<-i+1
}
#Value of GPSum as seen in work space is 2046

#### Question 18 ####
sum(2^(1:10))
#Value displayed in console is 2046

#### Question 19 ####
seq(20,50,5)
#Result: [1] 20 25 30 35 40 45 50

#### Question 20 ####
c(rep("example",10))

#### Question 21 ####
f = function(a,b,c) {
  if (b^2 >= 4*a*c){
    return(c((-1*b+sqrt(b^2-4*a*c))/(2*a),(-1*b-sqrt(b^2-4*a*c))/(2*a)))
  }
  else return ("No real roots as the determinant is -ve.")
}
#Sample 1
f(1,2,0)
#Result: [1]  0 -2
#Sample 2
f(2,5,6)
#Result: [1] "No real roots as the determinant is -ve."
