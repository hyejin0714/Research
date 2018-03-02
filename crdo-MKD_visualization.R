setwd("C:/Users/hyejin/Desktop/INTERN/data")
install.packages("ggplots2")
library(ggplot2)

mkd <-read.csv("crdo-MKD.csv", header= TRUE)
View(mkd)
cl <- mkd[which(mkd$Language == "CL"),]
cl

str(cl)
mkd$span <- ""

cl.rows <- which(mkd$Language == "CL")
mkd.notcl.rows <- which(mkd$Language == "L")

init = 1
a = 1
mkd$span[a] = 1

while (a+1 <= nrow(mkd)){
  if (mkd$Language[a] == mkd$Language[a+1]){
    mkd$span[a+1] <- mkd$span[a]
  }else{
    mkd$span[a+1] <- as.integer(mkd$span[a]) + 1
  }
  a = a +1 
}


View(mkd)


span <- as.integer(mkd$span)
span

barplot(table(span), xlab= "Span length", ylab="Span frequency", main = "Span distribution of crdo-MKD", col= c("blue","red"), border = c("blue","red"))
legend("topleft", legend = c("L", "CL"), fill = c("blue", "red"), cex=0.5, space = 1)

barplot(table(span), xlab= "Span length", ylab="Span frequency", main = "Span distribution of crdo-MKD", col = c("blue","red"), border = c("blue", "red"), log="y", space= 3)
legend("top", legend = c("L", "CL"), fill = c("blue", "red"), cex=0.5)
