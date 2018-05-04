setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.c6 <- read_tsv("crdo-SVM_CIGALE.tsv")

View(svm.c6)

summary(svm.c6)

svm.c6$span <- ""

init = 1
a = 1
svm.c6$span[a] = 1

while(a+1 <= nrow(svm.c6)) {
  if (svm.c6$Language[a] == svm.c6$Language[a+1]){
    svm.c6$span[a+1] <- svm.c6$span[a]} 
  else {svm.c6$span[a+1] <- as.integer(svm.c6$span[a])+1
  } 
  a= a+1
}


View(svm.c6)

span <- as.integer(svm.c6$span)

ftable(svm.c6$Language ~ span)
A <- cbind(ftable(svm.c6$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,31,by=2)
all.even = seq(2,31,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",31/2),rep("L",31/2+1))
C


svm.c6.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(0,31,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.c6.multi.bar
svm.c6.multi.bar + ggtitle("Span distribution of crdo-SVM_CIGALE")

max(table(span))

svm.c6.multi.bar + ggtitle("Span distribution of crdo-SVM_CIGALE") + ylim(-10,20)