setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.g5 <- read_tsv("crdo-SVM_GUERRE.tsv")

View(svm.g5)

summary(svm.g5)

svm.g5$span <- ""

init = 1
a = 1
svm.g5$span[a] = 1

while(a+1 <= nrow(svm.g5)) {
  if (svm.g5$Language[a] == svm.g5$Language[a+1]){
    svm.g5$span[a+1] <- svm.g5$span[a]} 
  else {svm.g5$span[a+1] <- as.integer(svm.g5$span[a])+1
  } 
  a= a+1
}


View(svm.g5)

span <- as.integer(svm.g5$span)

ftable(svm.g5$Language ~ span)
A <- cbind(ftable(svm.g5$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,288,by=2)
all.even = seq(2,288,by=2)

group.cl <- C[all.odd,]
group.l <- C[all.even,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.odd,all.even)
cl <- as.integer(C$CL[all.odd])
l<- as.integer(C$L[all.even])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",288/2),rep("L",288/2))
C


svm.g5.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,288,by=20)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.g5.multi.bar
svm.g5.multi.bar + ggtitle("Span distribution of crdo-SVM_GUERRE")

max(table(span))

svm.g5.multi.bar + ggtitle("Span distribution of crdo-SVM_GUERRE") + ylim(-10,55)