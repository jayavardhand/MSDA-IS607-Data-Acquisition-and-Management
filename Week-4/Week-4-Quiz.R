#Jayavardhan Daripineni

#### Question 1 ####
setwd ('C:/Users/jdaripin/Documents/IS607 - Data Acquisition and Management')
m <- read.table("movies.tab", sep="\t", header=TRUE, quote="", comment="")

opar=par(ps=10)
hist(m$year - m$year %% 10,  xlab='Decade', ylab='# of Movies', col=rainbow(12)
     ,main='Movies per Decade', labels=TRUE
     ,cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1)
opar


#### Question 2 ####
require(plyr)

#Action rating trend
Action <- m[m$Action == 1,][c('rating','year')]
ActionPlot <- ddply(Action, .(year), summarize, mean = round(mean(rating), 2))
scatter.smooth(ActionPlot, span=2/10, lpars = list(col = "red", lwd = 3, lty = 3))
#Action movies look like hitting peak between 1920s and 1940s and ever 
#decreasing after that till 1990s and hitting a bottom there and have 
#an increasing trend after that.

#Animation rating trend
Animation <- m[m$Animation == 1,][c('rating','year')]
AnimationPlot <- ddply(Animation, .(year), summarize, mean = round(mean(rating), 2))
scatter.smooth(AnimationPlot, span=2/10, lpars = list(col = "red", lwd = 3, lty = 3))
#Animation movies have a consistent trend between 6 and 7.


#Comedy rating trend
Comedy <- m[m$Comedy == 1,][c('rating','year')]
ComedyPlot <- ddply(Comedy, .(year), summarize, mean = round(mean(rating), 2))
scatter.smooth(ComedyPlot, span=2/10, lpars = list(col = "red", lwd = 3, lty = 3))
#The trend for Comedy movies appear similar to Action movies above.


#Drama rating trend
Drama <- m[m$Drama == 1,][c('rating','year')]
DramaPlot <- ddply(Drama, .(year), summarize, mean = round(mean(rating), 2))
scatter.smooth(DramaPlot, span=2/10, lpars = list(col = "red", lwd = 3, lty = 3))
#Appears to be consistent around 6 with minor fluctuations


#Documentary rating trend
Documentary <- m[m$Documentary == 1,][c('rating','year')]
DocumentaryPlot <- ddply(Documentary, .(year), summarize, mean = round(mean(rating), 2))
scatter.smooth(DocumentaryPlot, span=2/10, lpars = list(col = "red", lwd = 3, lty = 3))
#Are on an increasing trend post 2000s moving toward 7 and upwards.

#Romance rating trend
Romance <- m[m$Romance == 1,][c('rating','year')]
RomancePlot <- ddply(Romance, .(year), summarize, mean = round(mean(rating), 2))
scatter.smooth(RomancePlot, span=2/10, lpars = list(col = "red", lwd = 3, lty = 3))
#Peaked at 1920 and hit the bottom during 2000s and appears to be increasing after that

#Short rating trend
Short <- m[m$Short == 1,][c('rating','year')]
ShortPlot <- ddply(Short, .(year), summarize, mean = round(mean(rating), 2))
scatter.smooth(ShortPlot, span=2/10, lpars = list(col = "red", lwd = 3, lty = 3))
#See to have consistent rating between 6 and 7 but is slowly moving toward 7.

#### Question 3 ####
#Lets check how the ratings scatter
LengthRating <- m[c('rating','length')]
d <- ggplot(LengthRating, aes(length, rating))
d + geom_point()

#From the above plot, we see there are some outliers and ratings for movies of length > 500 are sparse.
#So lets plot for length < 500
LengthRating <- m[m$length < 500,][c('rating','length')]
d <- ggplot(LengthRating, aes(length, rating))
d + geom_point()

#From above, we see data points are most concentrated for movies around 100 mins and that are rated betwen 6 to 8
#Lets do a heat map or hex bin plot to see the concentration detail
install.packages('hexbin')
library(hexbin)
hexbinplot(length ~ rating,
           data=LengthRating, trans = NULL, inv = NULL,
           type=c("g", "r"))

#We now see, the most concentration is around ratings 5 to 7 and for movies between 90-100 mins.
#Also, there is something similar for short movies of around 10 mins with ratings 6-8.

#### Question 4 ####

#Let's plot for all the combinations of Genres (filtering for length<500 to avoid outliers)
#1.Preparing Data (m is populated in Question 1):
LengthGenre <- m[m$length < 500,][c('length','Action','Animation','Comedy','Drama','Documentary','Romance','Short')]
LengthGenre[,'Genre'] <- NA

f <- function(x){
  if (length(names(x[which(x %in% c(1))])) > 0){
    d <- names(x[which(x %in% c(1) & !(names(x) %in% c('length','Genre')))])
    paste(substr(d,1,2),collapse=",")
  }
}

LengthGenre$Genre <- apply(LengthGenre,1,f)
LengthGenre <- LengthGenre[,!(names(LengthGenre) %in% c('Action','Animation','Comedy','Drama','Documentary','Romance','Short'))]
LengthGenre$Genre[unlist(lapply(LengthGenre$Genre, is.null))] <- NA
LengthGenre$Genre <- unlist(LengthGenre$Genre)

#2.Plotting:
library(ggplot2)
ggplot(LengthGenre, aes(Genre,length)) + geom_point()

boxplot(length ~ Genre, data=LengthGenre, xlab='Genre', ylab='length')

#From the above graphs, it appears that each combination of genre is 
#concentrated around particular lenght but its not clear as the graph is 
#densely populated. So lets examine the summary statistics individually
LengthGenre <- m[c('length','Action','Animation','Comedy','Drama','Documentary','Romance','Short')]
summary(LengthGenre[which(LengthGenre$Action %in% c(1)),]$length)
summary(LengthGenre[which(LengthGenre$Animation %in% c(1)),]$length)
summary(LengthGenre[which(LengthGenre$Comedy %in% c(1)),]$length)
summary(LengthGenre[which(LengthGenre$Drama %in% c(1)),]$length)
summary(LengthGenre[which(LengthGenre$Documentary %in% c(1)),]$length)
summary(LengthGenre[which(LengthGenre$Romance %in% c(1)),]$length)

#From the above, we notice, for example the Mean length for Genre 'Romance' is 
#99 mins which appears to be appropriate considering the 1st and 3rd quartile 
#values. Similary, 'Animation' movies have mean length of 20 approximately. 
#Now lets see if a movie which is both Romance and Animation falls in between 
#these two lengths?

summary(LengthGenre[which(LengthGenre$Animation %in% c(1) & LengthGenre$Romance %in% c(1)),]$length)

Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
2.0    22.0    77.5    67.2    94.5   180.0 

#It seems that animation movies in general are small with mean lenght of 
#20 but when the movies are Romance as well, the length appears to get longer.
#This is a sample of analysis and it appears that length and genre are related.


#### Question 5 ####
T <- m[m$length<500,]
install.packages('hexbin')
library(hexbin)
hexbinplot(votes ~ length,
           data=T, trans = NULL, inv = NULL, xlim = c(0,500), ylim=c(0,10000),
           type=c("g", "r"))

hexbinplot(votes ~ (budget/length),
           data=T, trans = NULL, inv = NULL, xlim = c(0,9999999), ylim=c(0,10000),
           type=c("g", "r"))

#There appears to be a weak linear relationship between budget per unit length 
#of the movie and the number of votes.
