setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.g <- read_tsv("crdo-SVM_GAMBADE.tsv")

View(svm.g)

summary(svm.g)

svm.g$span <- ""

init = 1
a = 1
svm.g$span[a] = 1

while(a+1 <= nrow(svm.g)) {
  if (svm.g$Language[a] == svm.g$Language[a+1]){
    svm.g$span[a+1] <- svm.g$span[a]} 
  else {svm.g$span[a+1] <- as.integer(svm.g$span[a])+1
  } 
  a= a+1
}


View(svm.g)

span <- as.integer(svm.g$span)

ftable(svm.g$Language ~ span)
A <- cbind(ftable(svm.g$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,42,by=2)
all.even = seq(2,42,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",42/2),rep("L",42/2))
C


svm.g.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,42,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.g.multi.bar
svm.g.multi.bar + ggtitle("Span distribution of crdo-SVM_GAMBADE")

max(table(span))

svm.g.multi.bar + ggtitle("Span distribution of crdo-SVM_GAMBADE") + ylim(-5,40)