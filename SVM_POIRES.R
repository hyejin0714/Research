setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.p4 <- read_tsv("crdo-SVM_POIRES.tsv")

View(svm.p4)

summary(svm.p4)

svm.p4$span <- ""

init = 1
a = 1
svm.p4$span[a] = 1

while(a+1 <= nrow(svm.p4)) {
  if (svm.p4$Language[a] == svm.p4$Language[a+1]){
    svm.p4$span[a+1] <- svm.p4$span[a]} 
  else {svm.p4$span[a+1] <- as.integer(svm.p4$span[a])+1
  } 
  a= a+1
}


View(svm.p4)

span <- as.integer(svm.p4$span)

ftable(svm.p4$Language ~ span)
A <- cbind(ftable(svm.p4$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,217,by=2)
all.even = seq(2,217,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",217/2),rep("L",217/2+1))
C


svm.p4.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,217,by=20)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.p4.multi.bar
svm.p4.multi.bar + ggtitle("Span distribution of crdo-SVM_POIRES")

max(table(span))

svm.p4.multi.bar + ggtitle("Span distribution of crdo-SVM_POIRES") + ylim(-10,30)