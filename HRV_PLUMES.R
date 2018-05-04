setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

hrv.p <- read_tsv("crdo-HRV_PLUMES.tsv")
View(hrv.p)

summary(hrv.mar)


hrv.p$span <- ""

init = 1
a = 1
hrv.p$span[a] = 1

while(a+1 <= nrow(hrv.p)) {
  if (hrv.p$Language[a] == hrv.p$Language[a+1]){
    hrv.p$span[a+1] <- hrv.p$span[a]} 
  else {hrv.p$span[a+1] <- as.integer(hrv.p$span[a])+1
  } 
  a= a+1
}

View(hrv.p)

span <- as.integer(hrv.p$span)


ftable(hrv.p$Language ~ span)
A <- cbind(ftable(hrv.p$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C


str(C)

all.odd = seq(1,14,by=2)
all.even = seq(2,14,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",14/2),rep("L",14/2))
C


hrv.p.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

hrv.p.multi.bar
hrv.p.multi.bar + ggtitle("Span distribution of crdo-HRV_PLUMES")

