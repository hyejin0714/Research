setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.f4 <- read_tsv("crdo-SVM_FILLE2.tsv")

View(svm.f4)

summary(svm.f4)

svm.f4$span <- ""

init = 1
a = 1
svm.f4$span[a] = 1

while(a+1 <= nrow(svm.f4)) {
  if (svm.f4$Language[a] == svm.f4$Language[a+1]){
    svm.f4$span[a+1] <- svm.f4$span[a]} 
  else {svm.f4$span[a+1] <- as.integer(svm.f4$span[a])+1
  } 
  a= a+1
}


View(svm.f4)

span <- as.integer(svm.f4$span)

ftable(svm.f4$Language ~ span)
A <- cbind(ftable(svm.f4$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,161,by=2)
all.even = seq(2,161,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",161/2),rep("L",161/2+1))
C


svm.f4.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,161,by=20)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.f4.multi.bar
svm.f4.multi.bar + ggtitle("Span distribution of crdo-SVM_FILLE2")

max(table(span))

svm.f4.multi.bar + ggtitle("Span distribution of crdo-SVM_FILLE2") + ylim(-15,30)