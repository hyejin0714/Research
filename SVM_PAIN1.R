setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.p1 <- read_tsv("crdo-SVM_PAIN1.tsv")

View(svm.p1)

summary(svm.p1)

svm.p1$span <- ""

init = 1
a = 1
svm.p1$span[a] = 1

while(a+1 <= nrow(svm.p1)) {
  if (svm.p1$Language[a] == svm.p1$Language[a+1]){
    svm.p1$span[a+1] <- svm.p1$span[a]} 
  else {svm.p1$span[a+1] <- as.integer(svm.p1$span[a])+1
  } 
  a= a+1
}


View(svm.p1)

span <- as.integer(svm.p1$span)

ftable(svm.p1$Language ~ span)
A <- cbind(ftable(svm.p1$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,177,by=2)
all.even = seq(2,177,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",177/2),rep("L",177/2+1))
C


svm.p1.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,177,by=20)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.p1.multi.bar
svm.p1.multi.bar + ggtitle("Span distribution of crdo-SVM_PAIN1")

max(table(span))

svm.p1.multi.bar + ggtitle("Span distribution of crdo-SVM_PAIN1") + ylim(-10,30)