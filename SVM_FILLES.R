setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.f6 <- read_tsv("crdo-SVM_FILLES.tsv")

View(svm.f6)

summary(svm.f6)

svm.f6$span <- ""

init = 1
a = 1
svm.f6$span[a] = 1

while(a+1 <= nrow(svm.f6)) {
  if (svm.f6$Language[a] == svm.f6$Language[a+1]){
    svm.f6$span[a+1] <- svm.f6$span[a]} 
  else {svm.f6$span[a+1] <- as.integer(svm.f6$span[a])+1
  } 
  a= a+1
}


View(svm.f6)

span <- as.integer(svm.f6$span)

ftable(svm.f6$Language ~ span)
A <- cbind(ftable(svm.f6$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,59,by=2)
all.even = seq(2,59,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",59/2),rep("L",59/2+1))
C


svm.f6.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,59,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.f6.multi.bar
svm.f6.multi.bar + ggtitle("Span distribution of crdo-SVM_FILLES")

max(table(span))

svm.f6.multi.bar + ggtitle("Span distribution of crdo-SVM_FILLES") + ylim(-15,20)