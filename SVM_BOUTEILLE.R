setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.b3 <- read_tsv("crdo-SVM_BOUTEILLE.tsv")

View(svm.b3)

summary(svm.b3)

svm.b3$span <- ""

init = 1
a = 1
svm.b3$span[a] = 1

while(a+1 <= nrow(svm.b3)) {
  if (svm.b3$Language[a] == svm.b3$Language[a+1]){
    svm.b3$span[a+1] <- svm.b3$span[a]} 
  else {svm.b3$span[a+1] <- as.integer(svm.b3$span[a])+1
  } 
  a= a+1
}


View(svm.b3)

span <- as.integer(svm.b3$span)

ftable(svm.b3$Language ~ span)
A <- cbind(ftable(svm.b3$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,55,by=2)
all.even = seq(2,55,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",55/2),rep("L",55/2+1))
C


svm.b3.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.b3.multi.bar
svm.b3.multi.bar + ggtitle("Span distribution of crdo-SVM_BOUTEILLE")

max(table(span))

svm.b3.multi.bar + ggtitle("Span distribution of crdo-SVM_BOUTEILLE") + ylim(-10,20)