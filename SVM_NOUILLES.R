setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.n <- read_tsv("crdo-SVM_NOUILLES.tsv")

View(svm.n)

summary(svm.n)

svm.n$span <- ""

init = 1
a = 1
svm.n$span[a] = 1

while(a+1 <= nrow(svm.n)) {
  if (svm.n$Language[a] == svm.n$Language[a+1]){
    svm.n$span[a+1] <- svm.n$span[a]} 
  else {svm.n$span[a+1] <- as.integer(svm.n$span[a])+1
  } 
  a= a+1
}


View(svm.n)

span <- as.integer(svm.n$span)

ftable(svm.n$Language ~ span)
A <- cbind(ftable(svm.n$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,141,by=2)
all.even = seq(2,141,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",141/2),rep("L",141/2+1))
C


svm.n.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,141,by=20)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.n.multi.bar
svm.n.multi.bar + ggtitle("Span distribution of crdo-SVM_NOUILLES")

max(table(span))

svm.n.multi.bar + ggtitle("Span distribution of crdo-SVM_NOUILLES") + ylim(-10,40)