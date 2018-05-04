setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.r <- read_tsv("crdo-SVM_RECOLTE.tsv")

View(svm.r)

summary(svm.r)

svm.r$span <- ""

init = 1
a = 1
svm.r$span[a] = 1

while(a+1 <= nrow(svm.r)) {
  if (svm.r$Language[a] == svm.r$Language[a+1]){
    svm.r$span[a+1] <- svm.r$span[a]} 
  else {svm.r$span[a+1] <- as.integer(svm.r$span[a])+1
  } 
  a= a+1
}


View(svm.r)

span <- as.integer(svm.r$span)

ftable(svm.r$Language ~ span)
A <- cbind(ftable(svm.r$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,91,by=2)
all.even = seq(2,91,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",91/2),rep("L",91/2+1))
C


svm.r.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,91,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.r.multi.bar
svm.r.multi.bar + ggtitle("Span distribution of crdo-SVM_RECOLTE")

max(table(span))

svm.r.multi.bar + ggtitle("Span distribution of crdo-SVM_RECOLTE") + ylim(-5,20)
