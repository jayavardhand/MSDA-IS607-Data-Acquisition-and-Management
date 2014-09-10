#Jayavardhan Daripineni

#### Question 1 ####
f <- function(v){
  mean(v)
}

f(c(4,6,2)) #Result: 4

#### Question 2 ####
f <- function(v){
  mean(v, na.rm = TRUE)
}

#Example vector with one missing value
f(c(4,6,NA)) #Result: 5

#### Question 3 ####
gcd <- function(a,b){
  d<-i<-1
  while (i <= (min(a,b))){
    if (a %% i == 0 & b %% i == 0){
      d <- i
    }
    i <- i + 1
  }
  d
}
#Sample
gcd(28,42) #Result: 14

#### Question 4 ####
#Using Euclidian algorithm. Reference: http://en.wikipedia.org/wiki/Euclidean_algorithm#Continued_fractions
gcd <- function(a,b){
  while (b != 0){
    t <- b
    b <- a %% b
    a <- t
  }
  a
}
#Sample
gcd(28,42) #Result: 14

#### Question 5 ####
f <- function (x,y){
  (x^2)*y + 2*x*y - x*(y^2)
}
#Sample
f(4,3) #Result: 36

#### Question 6 ####
setwd ('C:/Users/jdaripin/Documents/IS607 - Data Acquisition and Management')
PriceData<- read.delim("week-3-price-data.csv", sep=",", header = TRUE, stringsAsFactors = FALSE)
MakeModelData<- read.delim("week-3-make-model-data.csv", sep=",", header = TRUE, stringsAsFactors = FALSE)
merge(PriceData, MakeModelData, by.x = "ModelNumber", by.y = "ModelNumber")
#Result: 27 observations. This is as expected as the Model Number 23120 should be excluded

#### Question 7 ####
LeftJoin <- merge(PriceData, MakeModelData, by.x = "ModelNumber", by.y = "ModelNumber", all.x = TRUE)
LeftJoin
#Result: 28 records with NA for columns of MakeModelData for ModelNumber 23120

#### Question 8 ####
subset(LeftJoin, (Year == 2010))

#### Question 9 ####
ExpensiveRed <- subset(LeftJoin, (toupper(Color) == 'RED') & (Price > 10000))
ExpensiveRed

#### Question 10 ####
ExpensiveRed <- subset(ExpensiveRed, select = -c(ModelNumber,Color))
ExpensiveRed

#### Question 11 ####
CharLength <- function(L){
  nchar(L)
}
#Sample
CharLength(c('Hello','World','HowAreYou?'))
#Result: 5  5 10

#### Question 12 ####
Concat <- function(A,B){
  if (length(A) != length(B)){
    stop("Vectors are NOT of equal length and cannot be concatenated.!")
  }
  paste(A,B)  
}

#Samples:
A = c('Bill', 'George', 'Michelle')
B = c('Clinton', 'Bush', 'Obama')
Concat(A,B) #Result: "Bill Hello"   "George World" "Michelle Hi"
Concat(A,c('Hello','World')) #Result: Error in Concat(A, c("Hello", "World")) : Vectors are NOT of equal length and cannot be concatenated.!

#### Question 13 ####
require(stringr)
AfterVowel <- function(txt){
  FirstVowelPos <- which(unlist(strsplit(tolower(txt),NULL)) %in% c('a','e','i','o','u') %in% TRUE)[1]
  substring(txt,FirstVowelPos+1,FirstVowelPos+3)
}
VectorAfterVowel <- function(A){
  sapply(A, function(x) AfterVowel(x))
}

#Sample
A = c('Bill', 'Columbia', 'Rhythm', 'Seattle')
VectorAfterVowel(A)
#Result (Input in first line, output in second line.):
Bill Columbia   Rhythm  Seattle 
"ll"    "lum"       NA    "att" 

#### Question 14 ####
df <- data.frame(Month=c(10,12,3), Day=c(28,31,17), Year=c(88,12,99))
df$Date <- c(as.Date(paste(df$Month,df$Day,df$Year,sep="-"), "%m-%d-%y"))
df
#Result:
  Month Day Year       Date
1    10  28   88 1988-10-28
2    12  31   12 2012-12-31
3     3  17   99 1999-03-17

#### Question 15 ####
d <- as.Date("08-22-2014", "%m-%d-%y")
#Verify:
class(d) #Result: [1] "Date"

#### Question 16 ####
format(Sys.time(), "%m") #For Month Number
format(Sys.time(), "%B") #For Month Name

#### Question 17 ####
dates <- as.Date(c(as.numeric(as.Date("2005-01-01")): as.numeric(as.Date("2014-12-31"))), origin = "1970-01-01")
#Verification:
head(dates)
#[1] "2005-01-01" "2005-01-02" "2005-01-03" "2005-01-04" "2005-01-05" "2005-01-06"
tail(dates)
#[1] "2014-12-26" "2014-12-27" "2014-12-28" "2014-12-29" "2014-12-30" "2014-12-31"
