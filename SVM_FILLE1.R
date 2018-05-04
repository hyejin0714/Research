setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.f3 <- read_tsv("crdo-SVM_FILLE1.tsv")

View(svm.f3)

summary(svm.f3)

svm.f3$span <- ""

init = 1
a = 1
svm.f3$span[a] = 1

while(a+1 <= nrow(svm.f3)) {
  if (svm.f3$Language[a] == svm.f3$Language[a+1]){
    svm.f3$span[a+1] <- svm.f3$span[a]} 
  else {svm.f3$span[a+1] <- as.integer(svm.f3$span[a])+1
  } 
  a= a+1
}


View(svm.f3)

span <- as.integer(svm.f3$span)

ftable(svm.f3$Language ~ span)
A <- cbind(ftable(svm.f3$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,103,by=2)
all.even = seq(2,103,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",103/2),rep("L",103/2+1))
C


svm.f3.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,103,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.f3.multi.bar
svm.f3.multi.bar + ggtitle("Span distribution of crdo-SVM_FILLE1")

max(table(span))

svm.f3.multi.bar + ggtitle("Span distribution of crdo-SVM_FILLE1") + ylim(-10,40)