setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.c4 <- read_tsv("crdo-SVM_CHARRETTE.tsv")

View(svm.c4)

summary(svm.c4)

svm.c4$span <- ""

init = 1
a = 1
svm.c4$span[a] = 1

while(a+1 <= nrow(svm.c4)) {
  if (svm.c4$Language[a] == svm.c4$Language[a+1]){
    svm.c4$span[a+1] <- svm.c4$span[a]} 
  else {svm.c4$span[a+1] <- as.integer(svm.c4$span[a])+1
  } 
  a= a+1
}


View(svm.c4)

span <- as.integer(svm.c4$span)

ftable(svm.c4$Language ~ span)
A <- cbind(ftable(svm.c4$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,53,by=2)
all.even = seq(2,53,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",53/2),rep("L",53/2+1))
C


svm.c4.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(0,53,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.c4.multi.bar
svm.c4.multi.bar + ggtitle("Span distribution of crdo-SVM_CHARRETTE")

max(table(span))

svm.c4.multi.bar + ggtitle("Span distribution of crdo-SVM_CHARRETTE") + ylim(-10,30)