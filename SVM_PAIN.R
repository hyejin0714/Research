setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.p <- read_tsv("crdo-SVM_PAIN.tsv")

View(svm.p)

summary(svm.p)

svm.p$span <- ""

init = 1
a = 1
svm.p$span[a] = 1

while(a+1 <= nrow(svm.p)) {
  if (svm.p$Language[a] == svm.p$Language[a+1]){
    svm.p$span[a+1] <- svm.p$span[a]} 
  else {svm.p$span[a+1] <- as.integer(svm.p$span[a])+1
  } 
  a= a+1
}


View(svm.p)

span <- as.integer(svm.p$span)

ftable(svm.p$Language ~ span)
A <- cbind(ftable(svm.p$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,71,by=2)
all.even = seq(2,71,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",71/2),rep("L",71/2+1))
C


svm.p.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,71,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.p.multi.bar
svm.p.multi.bar + ggtitle("Span distribution of crdo-SVM_PAIN")

max(table(span))

svm.p.multi.bar + ggtitle("Span distribution of crdo-SVM_PAIN") + ylim(-10,30)