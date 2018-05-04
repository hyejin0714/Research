setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.t <- read_tsv("crdo-SVM_TABAC.tsv")

View(svm.t)

summary(svm.t)

svm.t$span <- ""

init = 1
a = 1
svm.t$span[a] = 1

while(a+1 <= nrow(svm.t)) {
  if (svm.t$Language[a] == svm.t$Language[a+1]){
    svm.t$span[a+1] <- svm.t$span[a]} 
  else {svm.t$span[a+1] <- as.integer(svm.t$span[a])+1
  } 
  a= a+1
}


View(svm.t)

span <- as.integer(svm.t$span)

ftable(svm.t$Language ~ span)
A <- cbind(ftable(svm.t$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,183,by=2)
all.even = seq(2,183,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",183/2),rep("L",183/2+1))
C


svm.t.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,183,by=20)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.t.multi.bar
svm.t.multi.bar + ggtitle("Span distribution of crdo-SVM_TABAC")

max(table(span))

svm.t.multi.bar + ggtitle("Span distribution of crdo-SVM_TABAC") + ylim(-25,50)
