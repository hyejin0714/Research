setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.p2 <- read_tsv("crdo-SVM_PALATA.tsv")

View(svm.p2)

summary(svm.p2)

svm.p2$span <- ""

init = 1
a = 1
svm.p2$span[a] = 1

while(a+1 <= nrow(svm.p2)) {
  if (svm.p2$Language[a] == svm.p2$Language[a+1]){
    svm.p2$span[a+1] <- svm.p2$span[a]} 
  else {svm.p2$span[a+1] <- as.integer(svm.p2$span[a])+1
  } 
  a= a+1
}


View(svm.p2)

span <- as.integer(svm.p2$span)

ftable(svm.p2$Language ~ span)
A <- cbind(ftable(svm.p2$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,35,by=2)
all.even = seq(2,35,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",35/2),rep("L",35/2+1))
C


svm.p2.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,35,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.p2.multi.bar
svm.p2.multi.bar + ggtitle("Span distribution of crdo-SVM_PALATA")

max(table(span))

svm.p2.multi.bar + ggtitle("Span distribution of crdo-SVM_PALATA") + ylim(-10,25)