setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.c1 <- read_tsv("crdo-SVM_CHANCE.tsv")

View(svm.c1)

summary(svm.c1)

svm.c1$span <- ""

init = 1
a = 1
svm.c1$span[a] = 1

while(a+1 <= nrow(svm.c1)) {
  if (svm.c1$Language[a] == svm.c1$Language[a+1]){
    svm.c1$span[a+1] <- svm.c1$span[a]} 
  else {svm.c1$span[a+1] <- as.integer(svm.c1$span[a])+1
  } 
  a= a+1
}


View(svm.c1)

span <- as.integer(svm.c1$span)

ftable(svm.c1$Language ~ span)
A <- cbind(ftable(svm.c1$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,89,by=2)
all.even = seq(2,89,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",89/2),rep("L",89/2+1))
C


svm.c1.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.c1.multi.bar
svm.c1.multi.bar + ggtitle("Span distribution of crdo-SVM_CHANCE")

max(table(span))

svm.c1.multi.bar + ggtitle("Span distribution of crdo-SVM_CHANCE") + ylim(-10,20)