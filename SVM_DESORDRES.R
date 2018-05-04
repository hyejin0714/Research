setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.d <- read_tsv("crdo-SVM_DESORDRES.tsv")

View(svm.d)

summary(svm.d)

svm.d$span <- ""

init = 1
a = 1
svm.d$span[a] = 1

while(a+1 <= nrow(svm.d)) {
  if (svm.d$Language[a] == svm.d$Language[a+1]){
    svm.d$span[a+1] <- svm.d$span[a]} 
  else {svm.d$span[a+1] <- as.integer(svm.d$span[a])+1
  } 
  a= a+1
}


View(svm.d)

span <- as.integer(svm.d$span)

ftable(svm.d$Language ~ span)
A <- cbind(ftable(svm.d$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,107,by=2)
all.even = seq(2,107,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",107/2),rep("L",107/2+1))
C


svm.d.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,107,by=20)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.d.multi.bar
svm.d.multi.bar + ggtitle("Span distribution of crdo-SVM_DESORDRES")

max(table(span))

svm.d.multi.bar + ggtitle("Span distribution of crdo-SVM_DESORDRES") + ylim(-10,30)