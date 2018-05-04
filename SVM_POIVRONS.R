setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.p5 <- read_tsv("crdo-SVM_POIVRONS.tsv")

View(svm.p5)

summary(svm.p5)

svm.p5$span <- ""

init = 1
a = 1
svm.p5$span[a] = 1

while(a+1 <= nrow(svm.p5)) {
  if (svm.p5$Language[a] == svm.p5$Language[a+1]){
    svm.p5$span[a+1] <- svm.p5$span[a]} 
  else {svm.p5$span[a+1] <- as.integer(svm.p5$span[a])+1
  } 
  a= a+1
}


View(svm.p5)

span <- as.integer(svm.p5$span)

ftable(svm.p5$Language ~ span)
A <- cbind(ftable(svm.p5$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,67,by=2)
all.even = seq(2,67,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",67/2),rep("L",67/2+1))
C


svm.p5.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,67,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.p5.multi.bar
svm.p5.multi.bar + ggtitle("Span distribution of crdo-SVM_POIVRONS")

max(table(span))

svm.p5.multi.bar + ggtitle("Span distribution of crdo-SVM_POIVRONS") + ylim(-10,35)
