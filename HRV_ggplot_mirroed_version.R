setwd("C:/Users/hyejin/Desktop/INTERN/data")
install.packages("ggplots2")
library(ggplot2)

hrv <- read.csv("crdo-HRV.csv", header=TRUE)

hrv$span <- ""

init = 1
a = 1
hrv$span[a] = 1

while (a+1 <= nrow(hrv)){
  if (hrv$Language[a] == hrv$Language[a+1]){
    hrv$span[a+1] <- hrv$span[a]
  }else{
    hrv$span[a+1] <- as.integer(hrv$span[a]) + 1
  }
  a = a +1 
}

View(hrv)

span <- as.integer(hrv$span)

#data frame

ftable(hrv$Language ~ span)
A <- cbind(ftable(hrv$Language ~ span))
B <- A[,-1]
C <- data.frame(B)
colnames(C) <- c("CL","L")
C

all.odd = seq(1,92,by=2)
all.even = seq(2,92,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",46),rep("L",46))
C

## mirrored barplot 
hrv.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
    geom_bar(stat="identity", position="identity") + 
    scale_y_continuous(labels=abs)+
    scale_x_continuous(breaks=seq(1,92,by=10)) + 
    xlab("Span") + 
    ylab("Span Length") + 
    scale_fill_manual(values = c("red", "blue"))

hrv.multi.bar

hrv.multi.bar + ggtitle("Span distribution of crdo-HRV") + ylim(-50,230)



## looks real

hrv.multi.bar<- ggplot(C, aes(x= span, y= log(y), fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks=seq(1,92,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue"))

hrv.multi.bar

hrv.multi.bar + ggtitle("Span distribution of crdo-HRV")