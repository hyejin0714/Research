setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

hrv.v <- read_tsv("crdo-HRV_VIE.tsv")
View(hrv.v)

summary(hrv.v)

hrv.v$span <- ""

init = 1
a = 1
hrv.v$span[a] = 1

while(a+1 <= nrow(hrv.v)) {
  if (hrv.v$Language[a] == hrv.v$Language[a+1]){
    hrv.v$span[a+1] <- hrv.v$span[a]} 
  else {hrv.v$span[a+1] <- as.integer(hrv.v$span[a])+1
  } 
  a= a+1
}


View(hrv.v)

span <- as.integer(hrv.v$span)


ftable(hrv.v$Language ~ span)
A <- cbind(ftable(hrv.v$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,38,by=2)
all.even = seq(2,38,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",38/2),rep("L",38/2))
C


hrv.v.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

hrv.v.multi.bar
hrv.v.multi.bar + ggtitle("Span distribution of crdo-HRV_VIE")

max(table(span))

hrv.v.multi.bar + ggtitle("Span distribution of crdo-HRV_VIE") + ylim(-25,75)


