setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

hrv.mar <- read_tsv("crdo-HRV_MARIAGE.tsv")
View(hrv.mar)

summary(hrv.mar)


hrv.mar$span <- ""

init = 1
a = 1
hrv.mar$span[a] = 1

while(a+1 <= nrow(hrv.mar)) {
  if (hrv.mar$Language[a] == hrv.mar$Language[a+1]){
    hrv.mar$span[a+1] <- hrv.mar$span[a]} 
  else {hrv.mar$span[a+1] <- as.integer(hrv.mar$span[a])+1
    } 
  a= a+1
}

View(hrv.mar)

span <- as.integer(hrv.mar$span)


ftable(hrv.mar$Language ~ span)
A <- cbind(ftable(hrv.mar$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C


str(C)

all.odd = seq(1,15,by=2)
all.even = seq(2,15,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",15/2),rep("L",15/2+1))
C


hrv.mar.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

hrv.mar.multi.bar
hrv.mar.multi.bar + ggtitle("Span distribution of crdo-HRV_MARIAGE")



