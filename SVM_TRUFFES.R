setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.t2 <- read_tsv("crdo-SVM_TRUFFES.tsv")

View(svm.t2)

summary(svm.t2)

svm.t2$span <- ""

init = 1
a = 1
svm.t2$span[a] = 1

while(a+1 <= nrow(svm.t2)) {
  if (svm.t2$Language[a] == svm.t2$Language[a+1]){
    svm.t2$span[a+1] <- svm.t2$span[a]} 
  else {svm.t2$span[a+1] <- as.integer(svm.t2$span[a])+1
  } 
  a= a+1
}


View(svm.t2)

span <- as.integer(svm.t2$span)

ftable(svm.t2$Language ~ span)
A <- cbind(ftable(svm.t2$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,242,by=2)
all.even = seq(2,242,by=2)

group.cl <- C[all.odd,]
group.l <- C[all.even,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.odd,all.even)
cl <- as.integer(C$CL[all.odd])
l<- as.integer(C$L[all.even])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",242/2),rep("L",242/2))
C


svm.t2.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,242,by=20)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.t2.multi.bar
svm.t2.multi.bar + ggtitle("Span distribution of crdo-SVM_TRUFFES")

max(table(span))

svm.t2.multi.bar + ggtitle("Span distribution of crdo-SVM_TRUFFES") + ylim(-15,30)
