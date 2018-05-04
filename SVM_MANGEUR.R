setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.l6 <- read_tsv("crdo-SVM_MANGEUR.tsv")

View(svm.l6)

summary(svm.l6)

svm.l6$span <- ""

init = 1
a = 1
svm.l6$span[a] = 1

while(a+1 <= nrow(svm.l6)) {
  if (svm.l6$Language[a] == svm.l6$Language[a+1]){
    svm.l6$span[a+1] <- svm.l6$span[a]} 
  else {svm.l6$span[a+1] <- as.integer(svm.l6$span[a])+1
  } 
  a= a+1
}


View(svm.l6)

span <- as.integer(svm.l6$span)

ftable(svm.l6$Language ~ span)
A <- cbind(ftable(svm.l6$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,25,by=2)
all.even = seq(2,25,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",25/2),rep("L",25/2+1))
C


svm.l6.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,25,by=5)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.l6.multi.bar
svm.l6.multi.bar + ggtitle("Span distribution of crdo-SVM_MANGEUR")

max(table(span))

svm.l6.multi.bar + ggtitle("Span distribution of crdo-SVM_MANGEUR") + ylim(-5,20)