setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.f5 <- read_tsv("crdo-SVM_FILLE3.tsv")

View(svm.f5)

summary(svm.f5)

svm.f5$span <- ""

init = 1
a = 1
svm.f5$span[a] = 1

while(a+1 <= nrow(svm.f5)) {
  if (svm.f5$Language[a] == svm.f5$Language[a+1]){
    svm.f5$span[a+1] <- svm.f5$span[a]} 
  else {svm.f5$span[a+1] <- as.integer(svm.f5$span[a])+1
  } 
  a= a+1
}


View(svm.f5)

span <- as.integer(svm.f5$span)

ftable(svm.f5$Language ~ span)
A <- cbind(ftable(svm.f5$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,265,by=2)
all.even = seq(2,265,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",265/2),rep("L",265/2+1))
C


svm.f5.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,265,by=20)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.f5.multi.bar
svm.f5.multi.bar + ggtitle("Span distribution of crdo-SVM_FILLE3")

max(table(span))

svm.f5.multi.bar + ggtitle("Span distribution of crdo-SVM_FILLE3") + ylim(-5,40)