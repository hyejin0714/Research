setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.g4 <- read_tsv("crdo-SVM_GRENOUILLE1.tsv")

View(svm.g4)

summary(svm.g4)

svm.g4$span <- ""

init = 1
a = 1
svm.g4$span[a] = 1

while(a+1 <= nrow(svm.g4)) {
  if (svm.g4$Language[a] == svm.g4$Language[a+1]){
    svm.g4$span[a+1] <- svm.g4$span[a]} 
  else {svm.g4$span[a+1] <- as.integer(svm.g4$span[a])+1
  } 
  a= a+1
}


View(svm.g4)

span <- as.integer(svm.g4$span)

ftable(svm.g4$Language ~ span)
A <- cbind(ftable(svm.g4$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,9,by=2)
all.even = seq(2,9,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",9/2),rep("L",9/2+1))
C


svm.g4.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,9,by=1)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.g4.multi.bar
svm.g4.multi.bar + ggtitle("Span distribution of crdo-SVM_GRENOUILLE1")

max(table(span))

svm.g4.multi.bar + ggtitle("Span distribution of crdo-SVM_GRENOUILLE1") + ylim(-10,145)