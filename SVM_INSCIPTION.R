setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.I <- read_tsv("crdo-SVM_INSCRIPTION.tsv")

View(svm.I)

summary(svm.I)

svm.I$span <- ""

init = 1
a = 1
svm.I$span[a] = 1

while(a+1 <= nrow(svm.I)) {
  if (svm.I$Language[a] == svm.I$Language[a+1]){
    svm.I$span[a+1] <- svm.I$span[a]} 
  else {svm.I$span[a+1] <- as.integer(svm.I$span[a])+1
  } 
  a= a+1
}


View(svm.I)

span <- as.integer(svm.I$span)

ftable(svm.I$Language ~ span)
A <- cbind(ftable(svm.I$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,22,by=2)
all.even = seq(2,22,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",22/2),rep("L",22/2))
C


svm.I.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,22,by=5)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.I.multi.bar
svm.I.multi.bar + ggtitle("Span distribution of crdo-SVM_INSCRIPTION")

max(table(span))

svm.I.multi.bar + ggtitle("Span distribution of crdo-SVM_INSCRIPTION") + ylim(-5,25)