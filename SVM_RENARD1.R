setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.r2 <- read_tsv("crdo-SVM_RENARD1.tsv")

View(svm.r2)

summary(svm.r21)

svm.r2$span <- ""

init = 1
a = 1
svm.r2$span[a] = 1

while(a+1 <= nrow(svm.r2)) {
  if (svm.r2$Language[a] == svm.r2$Language[a+1]){
    svm.r2$span[a+1] <- svm.r2$span[a]} 
  else {svm.r2$span[a+1] <- as.integer(svm.r2$span[a])+1
  } 
  a= a+1
}


View(svm.r2)

span <- as.integer(svm.r2$span)

ftable(svm.r2$Language ~ span)
A <- cbind(ftable(svm.r2$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,125,by=2)
all.even = seq(2,125,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",125/2),rep("L",125/2+1))
C


svm.r2.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,125,by=20)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.r2.multi.bar
svm.r2.multi.bar + ggtitle("Span distribution of crdo-SVM_RENARD1")

max(table(span))

svm.r2.multi.bar + ggtitle("Span distribution of crdo-SVM_RENARD1") + ylim(-5,20)
