setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.e3 <- read_tsv("crdo-SVM_ENFANT.tsv")

View(svm.e3)

summary(svm.e3)

svm.e3$span <- ""

init = 1
a = 1
svm.e3$span[a] = 1

while(a+1 <= nrow(svm.e3)) {
  if (svm.e3$Language[a] == svm.e3$Language[a+1]){
    svm.e3$span[a+1] <- svm.e3$span[a]} 
  else {svm.e3$span[a+1] <- as.integer(svm.e3$span[a])+1
  } 
  a= a+1
}


View(svm.e3)

span <- as.integer(svm.e3$span)

ftable(svm.e3$Language ~ span)
A <- cbind(ftable(svm.e3$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,305,by=2)
all.even = seq(2,305,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",305/2),rep("L",305/2+1))
C


svm.e3.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,305,by=50)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.e3.multi.bar
svm.e3.multi.bar + ggtitle("Span distribution of crdo-SVM_ENFANT")

max(table(span))

svm.e3.multi.bar + ggtitle("Span distribution of crdo-SVM_ENFANT") + ylim(-10,35)