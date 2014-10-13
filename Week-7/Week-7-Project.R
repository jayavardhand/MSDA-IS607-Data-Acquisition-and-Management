#Jayavardhan Daripineni
#Load wiki hourly log data into PostgreSQL
install.packages("RPostgreSQL")
library(RPostgreSQL)

con <- dbConnect(drv,host="localhost",port="5432",user="postgres",password="WELCOME",dbname="dvdrental")

setwd ('C:/Users/Public/Documents')
names <- c('projectcode', 'pagename', 'pageviews', 'bytes')

#First hour data
m <- read.table("pagecounts-20141001-000000", sep=" ", col.names = names, header=FALSE, stringsAsFactors=FALSE, quote="", comment="")
dbWriteTable(con, "traffic_stats00",append=TRUE, row.names=FALSE, overwrite=FALSE, m)

#Second hour data
n <- read.table("pagecounts-20141001-010000", sep=" ", col.names = names, header=FALSE, stringsAsFactors=FALSE, quote="", comment="")
dbWriteTable(con, "traffic_stats01",append=TRUE, row.names=FALSE, overwrite=FALSE, n)

dbDisconnect(con)

#Graphically analyzing the data from both the hours using R. (This is totally independend of PostgreSQL data load done above.)
agg_m <- aggregate(pageviews ~ projectcode, data = m, FUN = sum)
agg_n <- aggregate(pageviews ~ projectcode, data = n, FUN = sum)

#Function to identify projectcodes that have a Period in them. These need to excluded as they are wiki projects.
require(stringr)
FindPeriod <- function(txt){
  match('.',unlist(strsplit(txt,NULL)))
}

#Exclude the projectcodes with Period in them with above function.
t <- sapply(agg_m$projectcode, function(x) FindPeriod(x))
agg_m_noPeriod <- data.frame(projectcode = names(t[which(t %in% c(NA))]), pageviews = agg_m$pageviews[which(t %in% c(NA))])
agg_m_noPeriod <- agg_m_noPeriod[with(agg_m_noPeriod, order(-pageviews)),]

t <- sapply(agg_n$projectcode, function(x) FindPeriod(x))
agg_n_noPeriod <- data.frame(projectcode = names(t[which(t %in% c(NA))]), pageviews = agg_n$pageviews[which(t %in% c(NA))])
agg_n_noPeriod <- agg_n_noPeriod[with(agg_n_noPeriod, order(-pageviews)),]

#Plot top 10 page views each hour:
agg_m_noPeriod$group <- 1
agg_n_noPeriod$group <- 2
agg <- rbind(head(agg_m_noPeriod,10), head(agg_n_noPeriod,10))
agg$group <- factor(agg$group)

library(ggplot2)
ggplot(agg, aes(x = projectcode, y = pageviews, group = group, color = group)) + geom_line()

#From the above plotted graph for top ten page views each hour, we can see that there are slight fluctuations and biggest change happend for 'ja' where the pageviews considerably increased during second hour.
