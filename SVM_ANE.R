setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.a <- read_tsv("crdo-SVM_ANE.tsv")

View(svm.a)

summary(svm.a)

svm.a$span <- ""

init = 1
a = 1
svm.a$span[a] = 1

while(a+1 <= nrow(svm.a)) {
  if (svm.a$Language[a] == svm.a$Language[a+1]){
    svm.a$span[a+1] <- svm.a$span[a]} 
  else {svm.a$span[a+1] <- as.integer(svm.a$span[a])+1
  } 
  a= a+1
}


View(svm.a)

span <- as.integer(svm.a$span)

ftable(svm.a$Language ~ span)
A <- cbind(ftable(svm.a$Language ~ span))
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
C$group <- c(rep("CL",59/2),rep("L",59/2+1))
C


svm.a.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.a.multi.bar
svm.a.multi.bar + ggtitle("Span distribution of crdo-SVM_ANE")

max(table(span))

svm.a.multi.bar + ggtitle("Span distribution of crdo-SVM_ANE") + ylim(-20,50)