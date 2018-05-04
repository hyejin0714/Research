install.packages("readr")
install.packages("pillar")
library(readr)

install.packages("ggplot2")
library(ggplot2)

setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

hrv.agr <- read_tsv("crdo-HRV_AGRICULTURE.tsv")

View(hrv.agr)
summary(hrv.agr)

hrv.agr$span <- ""

init=1
a=1
hrv.agr$span[a]=1

while (a+1 <= nrow(hrv.agr)){
  if (hrv.agr$Language[a] == hrv.agr$Language[a+1]){
    hrv.agr$span[a+1] <- hrv.agr$span[a]
  }else{
    hrv.agr$span[a+1] <- as.integer(hrv.agr$span[a]) + 1
  }
  a = a +1 
}


View(hrv.agr)

span <- as.integer(hrv.agr$span)


ftable(hrv.agr$Language ~ span)
A <- cbind(ftable(hrv.agr$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C


str(C)

all.odd = seq(1,9,by=2)
all.even = seq(2,9,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",8/2),rep("L",8/2+1))
C


hrv.agr.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

hrv.agr.multi.bar
hrv.agr.multi.bar + ggtitle("Span distribution of crdo-HRV_AGRICULTURE")

