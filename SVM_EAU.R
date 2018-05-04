setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

svm.e <- read_tsv("crdo-SVM_EAU.tsv")

View(svm.e)

summary(svm.e)

svm.e$span <- ""

init = 1
a = 1
svm.e$span[a] = 1

while(a+1 <= nrow(svm.e)) {
  if (svm.e$Language[a] == svm.e$Language[a+1]){
    svm.e$span[a+1] <- svm.e$span[a]} 
  else {svm.e$span[a+1] <- as.integer(svm.e$span[a])+1
  } 
  a= a+1
}


View(svm.e)

span <- as.integer(svm.e$span)

ftable(svm.e$Language ~ span)
A <- cbind(ftable(svm.e$Language ~ span))
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
C$group <- c(rep("CL",35/2),rep("L",35/2+1))
C


svm.e.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(1,35,by=10)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

svm.e.multi.bar
svm.e.multi.bar + ggtitle("Span distribution of crdo-SVM_EAU")

max(table(span))

svm.e.multi.bar + ggtitle("Span distribution of crdo-SVM_EAU") + ylim(-10,20)