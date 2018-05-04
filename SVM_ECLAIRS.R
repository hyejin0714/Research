setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.e2 <- read_tsv("crdo-SVM_ECLAIRS.tsv")

View(svm.e2)

summary(svm.e2)

svm.e2$span <- ""

init = 1
a = 1
svm.e2$span[a] = 1

while(a+1 <= nrow(svm.e2)) {
  if (svm.e2$Language[a] == svm.e2$Language[a+1]){
    svm.e2$span[a+1] <- svm.e2$span[a]} 
  else {svm.e2$span[a+1] <- as.integer(svm.e2$span[a])+1
  } 
  a= a+1
}


View(svm.e2)

span <- as.integer(svm.e2$span)

ftable(svm.e2$Language ~ span)
A <- cbind(ftable(svm.e2$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,57,by=2)
all.even = seq(2,57,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",57/2),rep("L",57/2+1))
C


svm.e2.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,57,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.e2.multi.bar
svm.e2.multi.bar + ggtitle("Span distribution of crdo-SVM_ECLAIRS")

max(table(span))

svm.e2.multi.bar + ggtitle("Span distribution of crdo-SVM_ECLAIRS") + ylim(-10,20)