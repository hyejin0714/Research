setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.v <- read_tsv("crdo-SVM_VESTON.tsv")

View(svm.v)

summary(svm.v)

svm.v$span <- ""

init = 1
a = 1
svm.v$span[a] = 1

while(a+1 <= nrow(svm.v)) {
  if (svm.v$Language[a] == svm.v$Language[a+1]){
    svm.v$span[a+1] <- svm.v$span[a]} 
  else {svm.v$span[a+1] <- as.integer(svm.v$span[a])+1
  } 
  a= a+1
}


View(svm.v)

span <- as.integer(svm.v$span)

ftable(svm.v$Language ~ span)
A <- cbind(ftable(svm.v$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,181,by=2)
all.even = seq(2,181,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",181/2),rep("L",181/2+1))
C


svm.v.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,181,by=20)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.v.multi.bar
svm.v.multi.bar + ggtitle("Span distribution of crdo-SVM_VESTON")

max(table(span))

svm.v.multi.bar + ggtitle("Span distribution of crdo-SVM_VESTON") + ylim(-15,35)
