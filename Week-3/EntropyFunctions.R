#Jayavardhan Daripineni

#### Entropy ####
entropy <- function(d){
  #Check for the number of unique values
  TotalValues <- length(d)
  NoOfUniqueValues <- length(unique(d))
  UniqueValues <- unique(d)
  E <- 0
  for (i in UniqueValues){
    #Calculate Probability of finding each value
    prb <- length(d[which(d %in% c(i))]) / TotalValues
    #Calculate Entropy by summing up in loop
    E <- E + -prb * log2(prb)
  }
  E
}

#### Infogain ####
infogain <- function(d,a){
  TotalValues <- length(d)
  E <- 0
  #Create list of partitions and calculate entropy for each partition in the same iteration, sum it up
  for (i in unique(a)){
    partition <- d[which(a %in% i)]
    E <- E + (length(partition) / TotalValues) * entropy(partition)
  }
  I <- entropy(d) - E
  I
}

#### Decide ####
decide <- function(df,n){
  AttrColNums <- c(1:length(df))[-n]
  gains <- mapply(infogain, df[n], df[AttrColNums], USE.NAMES = FALSE)
  names(gains) <- names(df)[c(1:length(df))[-n]]
  MaxColName <- names(which(gains == max(gains)))
  MaxColIndex <- which(names(df) == MaxColName)
  list(max=MaxColIndex, gains=gains)
}

#### Results: ####

#Uploaded the file to GitHub and loaded from the url
dataset <- read.delim("https://raw.githubusercontent.com/jayavardhand/MSDA-IS607-Data-Acquisition-and-Management/master/Week-3/entropy-test-file.csv", sep=",", header = TRUE, stringsAsFactors = FALSE)

#Call the above functions with the dataset
entropy(dataset$answer)
#[1] 0.9832692
infogain(dataset$answer,dataset$attr1)
#[1] 2.411565e-05
infogain(dataset$answer,dataset$attr2)
#[1] 0.2599038
infogain(dataset$answer,dataset$attr3)
#[1] 0.002432707
decide(dataset,4)
#$max
#[1] 2
#$gains
#attr1        attr2        attr3 
#2.411565e-05 2.599038e-01 2.432707e-03 
