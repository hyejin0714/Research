setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.c2 <- read_tsv("crdo-SVM_CHAPERON_ROUGE.tsv")

View(svm.c2)

summary(svm.c2)

svm.c2$span <- ""

init = 1
a = 1
svm.c2$span[a] = 1

while(a+1 <= nrow(svm.c2)) {
  if (svm.c2$Language[a] == svm.c2$Language[a+1]){
    svm.c2$span[a+1] <- svm.c2$span[a]} 
  else {svm.c2$span[a+1] <- as.integer(svm.c2$span[a])+1
  } 
  a= a+1
}


View(svm.c2)

span <- as.integer(svm.c2$span)

ftable(svm.c2$Language ~ span)
A <- cbind(ftable(svm.c2$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,110,by=2)
all.even = seq(2,110,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",110/2),rep("L",110/2))
C


svm.c2.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.c2.multi.bar
svm.c2.multi.bar + ggtitle("Span distribution of crdo-SVM_CHAPERON_ROUGE")

max(table(span))

svm.c2.multi.bar + ggtitle("Span distribution of crdo-SVM_CHAPERON_ROUGE") + ylim(-10,40)