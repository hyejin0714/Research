setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.g3 <- read_tsv("crdo-SVM_GENDARME.tsv")

View(svm.g3)

summary(svm.g3)

svm.g3$span <- ""

init = 1
a = 1
svm.g3$span[a] = 1

while(a+1 <= nrow(svm.g3)) {
  if (svm.g3$Language[a] == svm.g3$Language[a+1]){
    svm.g3$span[a+1] <- svm.g3$span[a]} 
  else {svm.g3$span[a+1] <- as.integer(svm.g3$span[a])+1
  } 
  a= a+1
}


View(svm.g3)

span <- as.integer(svm.g3$span)

ftable(svm.g3$Language ~ span)
A <- cbind(ftable(svm.g3$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,90,by=2)
all.even = seq(2,90,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",90/2),rep("L",90/2))
C


svm.g3.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,90,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.g3.multi.bar
svm.g3.multi.bar + ggtitle("Span distribution of crdo-SVM_GENDARME")

max(table(span))

svm.g3.multi.bar + ggtitle("Span distribution of crdo-SVM_GENDARME") + ylim(-5,15)