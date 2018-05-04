setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.h <- read_tsv("crdo-SVM_HUILE.tsv")

View(svm.h)

summary(svm.h)

svm.h$span <- ""

init = 1
a = 1
svm.h$span[a] = 1

while(a+1 <= nrow(svm.h)) {
  if (svm.h$Language[a] == svm.h$Language[a+1]){
    svm.h$span[a+1] <- svm.h$span[a]} 
  else {svm.h$span[a+1] <- as.integer(svm.h$span[a])+1
  } 
  a= a+1
}


View(svm.h)

span <- as.integer(svm.h$span)

ftable(svm.h$Language ~ span)
A <- cbind(ftable(svm.h$Language ~ span))
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
C$Language <- c(rep("CL",113/2),rep("L",113/2+1))
C


svm.h.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,113,by=20)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.h.multi.bar
svm.h.multi.bar + ggtitle("Span distribution of crdo-SVM_HUILE")

max(table(span))

svm.h.multi.bar + ggtitle("Span distribution of crdo-SVM_HUILE") + ylim(-5,25)