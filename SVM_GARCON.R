setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.g1 <- read_tsv("crdo-SVM_GARCON.tsv")

View(svm.g1)

summary(svm.g1)

svm.g1$span <- ""

init = 1
a = 1
svm.g1$span[a] = 1

while(a+1 <= nrow(svm.g1)) {
  if (svm.g1$Language[a] == svm.g1$Language[a+1]){
    svm.g1$span[a+1] <- svm.g1$span[a]} 
  else {svm.g1$span[a+1] <- as.integer(svm.g1$span[a])+1
  } 
  a= a+1
}


View(svm.g1)

span <- as.integer(svm.g1$span)

ftable(svm.g1$Language ~ span)
A <- cbind(ftable(svm.g1$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,105,by=2)
all.even = seq(2,105,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",105/2),rep("L",105/2+1))
C


svm.g1.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,105,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.g1.multi.bar
svm.g1.multi.bar + ggtitle("Span distribution of crdo-SVM_GARCON")

max(table(span))

svm.g1.multi.bar + ggtitle("Span distribution of crdo-SVM_GARCON") + ylim(-5,30)