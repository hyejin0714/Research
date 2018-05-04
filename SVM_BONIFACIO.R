setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.b1 <- read_tsv("crdo-SVM_BONIFACIO.tsv")

View(svm.b1)

summary(svm.b1)

svm.b1$span <- ""

init = 1
a = 1
svm.b1$span[a] = 1

while(a+1 <= nrow(svm.b1)) {
  if (svm.b1$Language[a] == svm.b1$Language[a+1]){
    svm.b1$span[a+1] <- svm.b1$span[a]} 
  else {svm.b1$span[a+1] <- as.integer(svm.b1$span[a])+1
  } 
  a= a+1
}


View(svm.b1)

span <- as.integer(svm.b1$span)

ftable(svm.b1$Language ~ span)
A <- cbind(ftable(svm.b1$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,63,by=2)
all.even = seq(2,63,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",63/2),rep("L",63/2+1))
C


svm.b1.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.b1.multi.bar
svm.b1.multi.bar + ggtitle("Span distribution of crdo-SVM_BONIFACIO")

max(table(span))

svm.b1.multi.bar + ggtitle("Span distribution of crdo-SVM_BONIFACIO") + ylim(-10,15)