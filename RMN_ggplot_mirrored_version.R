getwd()
setwd("C:/Users/hyejin/Desktop/INTERN/data")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

rmn <- read_tsv("crdo-RMN.tsv")
summary(rmn)

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


ftable(rmn$Language ~ span)
A <- cbind(ftable(rmn$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)


all.even = seq(2,673,by=2)
all.odd = seq(1,673,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl
group.l

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",672/2),rep("L",672/2+1))
C


rmn.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks=seq(1,672,by=50)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue"))

rmn.multi.bar
rmn.multi.bar + ggtitle("Span distribution of crdo-RMN")+ylim(-20,40)
