setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.f <- read_tsv("crdo-SVM_FEMME.tsv")

View(svm.f)

summary(svm.f)

svm.f$span <- ""

init = 1
a = 1
svm.f$span[a] = 1

while(a+1 <= nrow(svm.f)) {
  if (svm.f$Language[a] == svm.f$Language[a+1]){
    svm.f$span[a+1] <- svm.f$span[a]} 
  else {svm.f$span[a+1] <- as.integer(svm.f$span[a])+1
  } 
  a= a+1
}


View(svm.f)

span <- as.integer(svm.f$span)

ftable(svm.f$Language ~ span)
A <- cbind(ftable(svm.f$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,57,by=2)
all.even = seq(2,57,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$Language <- c(rep("CL",57/2),rep("L",57/2+1))
C


svm.f.multi.bar<- ggplot(C, aes(x= span, y= y , fill= Language)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,57,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.f.multi.bar
svm.f.multi.bar + ggtitle("Span distribution of crdo-SVM_FEMME")

max(table(span))

svm.f.multi.bar + ggtitle("Span distribution of crdo-SVM_FEMME") + ylim(-10,30)