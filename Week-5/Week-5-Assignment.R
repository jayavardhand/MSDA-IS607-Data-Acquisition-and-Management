#Jayavardhan Daripineni

#### Question 1 ####
1. What percentage of votes are for YES and for NO on whole?
2. What percentage of votes are from each city?
3. What is Age group wise share of votes on whole?

#### Question 2 ####
df <- data.frame(Vote=c('Yes','No'),Edinburgh16to24=c(80100,35900),
                 Edinburgh25Plus=c(143000,214800),Glasgow16to24=c(99400,43000),
                 Glasgow25Plus=c(150400,207000),stringsAsFactors='False')

#### Question 3 ####
#install.packages('tidyr')
#install.packages('dplyr')
require(tidyr)
require(dplyr)

df <- df %>%
  gather(key,value,Edinburgh16to24:Glasgow25Plus) %>%
  separate(key, c("city","age"),-7)

#### Question 4 ####
require(plyr)

#1:
ddply(df %>% spread(city,value),.(Vote), summarize, Percentage=round(sum(Edinburgh + Glasgow)/sum(df$value) * 100, 2))

  Vote Percentage
1   No      51.43
2  Yes      48.57

#2:
ddply(df, .(city), summarize, Percentage = round(sum(value)/sum(df$value) * 100, 2))

  city      Percentage
1 Edinburgh      48.66
2   Glasgow      51.34

#3:
ddply(df, .(age), summarize, Percentage = round(sum(value)/sum(df$value) * 100, 2))

  age     Percentage
1 16to24      26.54
2 25Plus      73.46

#### Question 5 ####
#The initial data frame that was structured in question 2 above to accomodate the 
#two records doesn't easily allow to run commands to answer the questions asked in 
#question 1 above. In order to make it easier, the data frame needed to be made longer from 
#wider and run the aggregation commands. So, yes, I'd definitely tidy the data and 
#implement the data frame to be long than wide. This will help us get a different 
#perspective on data by letting us ask new questions like what is ratio of 16-24 aged 
#group between Edinburgh and Glasgow etc. Though this appears to be easily answerable 
#from the initial data frame before tidying in this case, I think larger data sets make the 
#the need for tidying more apparent by presenting different perspectives.
