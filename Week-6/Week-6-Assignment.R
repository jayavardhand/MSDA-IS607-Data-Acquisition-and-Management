#install.packages("devtools")
#library(devtools)
#install_github("hadley/rvest")

library(rvest)
msft <- html("http://technet.microsoft.com/en-us/library/ee633746.aspx")
mdm <- msft %>%
  html_nodes("#mainBody .tableSection table")
html_table(mdm)
  

