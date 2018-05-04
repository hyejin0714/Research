setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)


hrv.gre <- read_tsv("crdo-HRV_GRENOUILLE2.tsv")

View(hrv)

summary(hrv.gre)

hrv.gre$span <- ""

init=1
a=1
hrv.gre$span[a]=1

while (a+1 <= nrow(hrv.gre)){
  if (hrv.gre$Language[a] == hrv.gre$Language[a+1]){
    hrv.gre$span[a+1] <- hrv.gre$span[a]
  }else{
    hrv.gre$span[a+1] <- as.integer(hrv.gre$span[a]) + 1
  }
  a = a +1 
}


View(hrv.gre)

span <- as.integer(hrv.gre$span)


ftable(hrv.gre$Language ~ span)
A <- cbind(ftable(hrv.gre$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C


str(C)

all.odd = seq(1,37,by=2)
all.even = seq(2,37,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",37/2),rep("L",37/2+1))
C



hrv.gre.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

hrv.gre.multi.bar
hrv.gre.multi.bar + ggtitle("Span distribution of crdo-HRV_GRENOUILLE2") + ylim(-50,160)


