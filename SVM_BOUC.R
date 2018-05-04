setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.b2 <- read_tsv("crdo-SVM_BOUC.tsv")

View(svm.b2)

summary(svm.b2)

svm.b2$span <- ""

init = 1
a = 1
svm.b2$span[a] = 1

while(a+1 <= nrow(svm.b2)) {
  if (svm.b2$Language[a] == svm.b2$Language[a+1]){
    svm.b2$span[a+1] <- svm.b2$span[a]} 
  else {svm.b2$span[a+1] <- as.integer(svm.b2$span[a])+1
  } 
  a= a+1
}


View(svm.b2)

span <- as.integer(svm.b2$span)

ftable(svm.b2$Language ~ span)
A <- cbind(ftable(svm.b2$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,195,by=2)
all.even = seq(2,195,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",195/2),rep("L",195/2+1))
C


svm.b2.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.b2.multi.bar
svm.b2.multi.bar + ggtitle("Span distribution of crdo-SVM_BOUC")

max(table(span))

svm.b2.multi.bar + ggtitle("Span distribution of crdo-SVM_BOUC") + ylim(-20,40)