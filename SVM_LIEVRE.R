setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.l1 <- read_tsv("crdo-SVM_LIEVRE.tsv")

View(svm.l1)

summary(svm.l1)

svm.l1$span <- ""

init = 1
a = 1
svm.l1$span[a] = 1

while(a+1 <= nrow(svm.l1)) {
  if (svm.l1$Language[a] == svm.l1$Language[a+1]){
    svm.l1$span[a+1] <- svm.l1$span[a]} 
  else {svm.l1$span[a+1] <- as.integer(svm.l1$span[a])+1
  } 
  a= a+1
}


View(svm.l1)

span <- as.integer(svm.l1$span)

ftable(svm.l1$Language ~ span)
A <- cbind(ftable(svm.l1$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,76,by=2)
all.even = seq(2,76,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",76/2),rep("L",76/2))
C


svm.l1.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,76,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.l1.multi.bar
svm.l1.multi.bar + ggtitle("Span distribution of crdo-SVM_LIEVRE")

max(table(span))

svm.l1.multi.bar + ggtitle("Span distribution of crdo-SVM_LIEVRE") + ylim(-5,40)