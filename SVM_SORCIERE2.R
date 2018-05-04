setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.s3 <- read_tsv("crdo-SVM_SORCIERE2.tsv")

View(svm.s3)

summary(svm.s3)

svm.s3$span <- ""

init = 1
a = 1
svm.s3$span[a] = 1

while(a+1 <= nrow(svm.s3)) {
  if (svm.s3$Language[a] == svm.s3$Language[a+1]){
    svm.s3$span[a+1] <- svm.s3$span[a]} 
  else {svm.s3$span[a+1] <- as.integer(svm.s3$span[a])+1
  } 
  a= a+1
}


View(svm.s3)

span <- as.integer(svm.s3$span)

ftable(svm.s3$Language ~ span)
A <- cbind(ftable(svm.s3$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,89,by=2)
all.even = seq(2,89,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",89/2),rep("L",89/2+1))
C


svm.s3.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,89,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.s3.multi.bar
svm.s3.multi.bar + ggtitle("Span distribution of crdo-SVM_SORCIERE2")

max(table(span))

svm.s3.multi.bar + ggtitle("Span distribution of crdo-SVM_SORCIERE2") + ylim(-10,30)
