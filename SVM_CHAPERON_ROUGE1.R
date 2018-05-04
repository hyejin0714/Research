setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.c3 <- read_tsv("crdo-SVM_CHAPERON_ROUGE1.tsv")

View(svm.c3)

summary(svm.c3)

svm.c3$span <- ""

init = 1
a = 1
svm.c3$span[a] = 1

while(a+1 <= nrow(svm.c3)) {
  if (svm.c3$Language[a] == svm.c3$Language[a+1]){
    svm.c3$span[a+1] <- svm.c3$span[a]} 
  else {svm.c3$span[a+1] <- as.integer(svm.c3$span[a])+1
  } 
  a= a+1
}


View(svm.c3)

span <- as.integer(svm.c3$span)

ftable(svm.c3$Language ~ span)
A <- cbind(ftable(svm.c3$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,140,by=2)
all.even = seq(2,140,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",140/2),rep("L",140/2))
C


svm.c3.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(0,140,by=20)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.c3.multi.bar
svm.c3.multi.bar + ggtitle("Span distribution of crdo-SVM_CHAPERON_ROUGE1")

max(table(span))

svm.c3.multi.bar + ggtitle("Span distribution of crdo-SVM_CHAPERON_ROUGE1") + ylim(-10,40)