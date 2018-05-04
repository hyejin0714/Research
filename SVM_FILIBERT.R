setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.f1 <- read_tsv("crdo-SVM_FILIBERT.tsv")

View(svm.f1)

summary(svm.f1)

svm.f1$span <- ""

init = 1
a = 1
svm.f1$span[a] = 1

while(a+1 <= nrow(svm.f1)) {
  if (svm.f1$Language[a] == svm.f1$Language[a+1]){
    svm.f1$span[a+1] <- svm.f1$span[a]} 
  else {svm.f1$span[a+1] <- as.integer(svm.f1$span[a])+1
  } 
  a= a+1
}


View(svm.f1)

span <- as.integer(svm.f1$span)

ftable(svm.f1$Language ~ span)
A <- cbind(ftable(svm.f1$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,71,by=2)
all.even = seq(2,71,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",71/2),rep("L",71/2+1))
C


svm.f1.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,71,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.f1.multi.bar
svm.f1.multi.bar + ggtitle("Span distribution of crdo-SVM_FILIBERT")

max(table(span))

svm.f1.multi.bar + ggtitle("Span distribution of crdo-SVM_FILIBERT") + ylim(-5,15)