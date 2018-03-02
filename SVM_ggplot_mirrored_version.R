getwd()
setwd("C:/Users/hyejin/Desktop/INTERN/data")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm <- read_tsv("crdo-SVM.tsv")
summary(svm)

svm$span <- ""

init = 1
a = 1
svm$span[a] = 1

while (a+1 <= nrow(svm)){
  if (svm$Language[a] == svm$Language[a+1]){
    svm$span[a+1] <- svm$span[a]
  }else{
    svm$span[a+1] <- as.integer(svm$span[a]) + 1
  }
  a = a +1 
}


View(svm)

span <- as.integer(svm$span)


ftable(svm$Language ~ span)
A <- cbind(ftable(svm$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)


all.even = seq(2,6649,by=2)
all.odd = seq(1,6649,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl
group.l

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",6648/2),rep("L",6648/2+1))
C


svm.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks=seq(1,6649,by=500)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue"))

svm.multi.bar
svm.multi.bar + ggtitle("Span distribution of crdo-RMN")+ylim(-50,150)
