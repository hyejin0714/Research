setwd("C:/Users/hyejin/Desktop/INTERN/data")
install.packages("ggplots2")
library(ggplot2)

rmn <-read.csv("crdo-RMN.csv", header= TRUE)
View(rmn)
cl <- rmn[which(rmn$Language == "CL"),]
cl
rmn$span <- ""

init = 1
a = 1
rmn$span[a] = 1

while (a+1 <= nrow(rmn)){
  if (rmn$Language[a] == rmn$Language[a+1]){
    rmn$span[a+1] <- rmn$span[a]
  }else{
    rmn$span[a+1] <- as.integer(rmn$span[a]) + 1
  }
  a = a +1 
}


View(rmn)


span <- as.integer(rmn$span)
span

barplot(table(span), xlab= "Span length", ylab="Span frequency", main = "Span distribution of crdo-RMN", col= c("blue","red"), border = c("blue","red"), ylim = c(0,40), space= 3)
legend("topright", legend = c("L", "CL"), fill = c("blue", "red"), cex=0.5)

barplot(table(span), xlab= "Span length", ylab="Span frequency", main = "Span distribution of crdo-RMN", col = c("blue","red"), border = c("blue", "red"), log="y", space= 3, ylim = log(c(0,40)))
legend("toprigt", legend = c("L", "CL"), fill = c("blue", "red"), cex=0.5)
