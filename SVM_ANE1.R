setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.a1 <- read_tsv("crdo-SVM_ANE1.tsv")

View(svm.a1)

summary(svm.a1)

svm.a1$span <- ""

init = 1
a = 1
svm.a1$span[a] = 1

while(a+1 <= nrow(svm.a1)) {
  if (svm.a1$Language[a] == svm.a1$Language[a+1]){
    svm.a1$span[a+1] <- svm.a1$span[a]} 
  else {svm.a1$span[a+1] <- as.integer(svm.a1$span[a])+1
  } 
  a= a+1
}


View(svm.a1)

span <- as.integer(svm.a1$span)

ftable(svm.a1$Language ~ span)
A <- cbind(ftable(svm.a1$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,35,by=2)
all.even = seq(2,35,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",35/2),rep("L",35/2+1))
C


svm.a1.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.a1.multi.bar
svm.a1.multi.bar + ggtitle("Span distribution of crdo-SVM_ANE1")

max(table(span))

svm.a1.multi.bar + ggtitle("Span distribution of crdo-SVM_ANE1") + ylim(-10,15)