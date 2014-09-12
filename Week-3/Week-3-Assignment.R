#Jayavardhan Daripineni

#### Question 1 ####
f <- function(d){
  length(which(d %in% NA))
}
#Example:
d <- c(3,5,78,23,NA,3,NA,63)
f(d)
#Result: 2

#### Question 2 ####
missingVal <- function(d){
  sapply(d,function(x) f(x)) #Using function from Question 1.
}

#Example
d <- data.frame(Name = c('A','D',NA,'Z'), Score = c(NA,45,1,56))
missingVal(d)
#Result:
Name Score 
1     1 

#### Question 3 ####
fMedian <- function(d){
  d <- sort(d)
  l <- length(d)
  IsOddLength <- l%%2
  if (IsOddLength == 1) Median <- d[((l+IsOddLength)/2)]
  else Median <- (d[l/2] + d[(l/2)+1])/2
  Median
}
fMean <- function(d){
  Missing <- length(which(d %in% c(NA)))
  d <- sort(d)
  l <- length(d)
  sum <- SD <- 0
  for (i in d){
    sum <- sum + i
  }
  Min <- d[1]
  Max <- d[l]
  Mean <- sum/l
  for (i in (d-Mean)^2){
    SD <- SD + i
  }
  SD <- sqrt(SD/(l-1))
  c(Min,Max,Mean,SD,Missing)
}

#Main function to calculate all
fStats <- function(d){
  Others <- fMean(d)
  d <- sort(d)
  l <- length(d)
  IsOddLength <- l%%2  
  Median <- fMedian(d)
  FirstQu <- fMedian(d[1:((l-IsOddLength)/2)])
  ThirdQu <- fMedian(d[(1+(l+IsOddLength)/2):l])
  Result <- c(Median, FirstQu, ThirdQu, Others)
  names(Result) <- c('Median','FirstQu','ThirdQu','Min','Max','Mean','SD','Missing')
  Result
}

#Example 1:
d <- c(3,5,8,2,6,10,5,1)
fStats(d)
#Result 1:
Median   FirstQu   ThirdQu       Min       Max      Mean        SD   Missing 
5.000000  2.500000  7.000000  1.000000 10.000000  5.000000  3.023716  0.000000 

#Example 2:
fStats(c(1,2,NA,4))
#Result 2:
Median  FirstQu  ThirdQu      Min      Max     Mean       SD  Missing 
2.000000 1.000000 4.000000 1.000000 4.000000 2.333333 1.527525 1.000000 

#### Question 4 ####
fFreq <- function(queue){
  Result <- c(length(unique(queue[-which(queue %in% c(NA))])))
  freq <- data.frame(table(queue))
  freq$queue <- as.character(freq$queue)
  Result <- c(Result, paste(freq[freq$Freq == max(freq$Freq),]$queue, collapse=' | '), max(freq$Freq))
  Result <- c(Result, length(which(queue %in% c(NA))))
  names(Result) <- c('UniqueValues','MostCommon','HighestFreq','Missing')
  Result
}

#Example:
queue <- c('James', 'Mary', 'Steve', 'Alex', 'Patricia','Alex','James','Alex','James',NA)
fFreq(queue)

#Result:
UniqueValues     MostCommon    HighestFreq        Missing 
         "5" "Alex | James"            "3"            "1" 

#### Question 5 ####
fCount <- function(d){
  nT <- length(d[which(d %in% c(TRUE))])
  nF <- length(d[which(d %in% c(FALSE))])
  Missing <- length(d[which(d %in% c(NA))])
  TP <- paste(nT, "of", length(d), collapse=" ")
  Result <- c(nT, nF, TP, Missing)
  names(Result) <- c('NoOfTrue', 'NoOfFalse', 'True_Proportion', 'Missing')
  Result
}
#Example
d <- c(FALSE,TRUE,FALSE,FALSE,NA)
fCount(d)

#Result:
NoOfTrue       NoOfFalse True_Proportion         Missing 
"1"             "3"        "1 of 5"             "1" 

#### Question 6 ####
fSummary <- function(df){
  Summary <- list(Marks = fStats(df$Marks), Student = fFreq(df$Student), Pass = fCount(df$Pass))
  Summary
}

#Example:
df <- data.frame(Student = c('James', 'Mary', 'Steve', 'Alex', 'Patricia'),
                 Marks = c(34,76,55,63,45),
                 Pass = c(FALSE,TRUE,TRUE,TRUE,FALSE))
fSummary(df)

#Result:
$Marks
Median  FirstQu  ThirdQu      Min      Max     Mean       SD  Missing 
55.00000 39.50000 69.50000 34.00000 76.00000 54.60000 16.16478  0.00000 

$Student
UniqueValues 
"0" 
MostCommon 
"Alex | James | Mary | Patricia | Steve" 
HighestFreq 
"1" 
Missing 
"0" 

$Pass
NoOfTrue       NoOfFalse True_Proportion         Missing 
"3"             "2"        "3 of 5"             "0" 
