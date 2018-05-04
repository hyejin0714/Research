setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.c5 <- read_tsv("crdo-SVM_CHATTE.tsv")

View(svm.c5)

summary(svm.c5)

svm.c5$span <- ""

init = 1
a = 1
svm.c5$span[a] = 1

while(a+1 <= nrow(svm.c5)) {
  if (svm.c5$Language[a] == svm.c5$Language[a+1]){
    svm.c5$span[a+1] <- svm.c5$span[a]} 
  else {svm.c5$span[a+1] <- as.integer(svm.c5$span[a])+1
  } 
  a= a+1
}


View(svm.c5)

span <- as.integer(svm.c5$span)

ftable(svm.c5$Language ~ span)
A <- cbind(ftable(svm.c5$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,113,by=2)
all.even = seq(2,113,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",113/2),rep("L",113/2+1))
C


svm.c5.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(0,113,by=20)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.c5.multi.bar
svm.c5.multi.bar + ggtitle("Span distribution of crdo-SVM_CHATTE")

max(table(span))

svm.c5.multi.bar + ggtitle("Span distribution of crdo-SVM_CHATTE") + ylim(-10,30)