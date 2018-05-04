setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

hrv.s <- read_tsv("crdo-HRV_SOURIS.tsv")
View(hrv.s)

summary(hrv.s)

hrv.s$span <- ""

init = 1
a = 1
hrv.s$span[a] = 1

while(a+1 <= nrow(hrv.s)) {
  if (hrv.s$Language[a] == hrv.s$Language[a+1]){
    hrv.s$span[a+1] <- hrv.s$span[a]} 
  else {hrv.s$span[a+1] <- as.integer(hrv.s$span[a])+1
  } 
  a= a+1
}


View(hrv.s)

span <- as.integer(hrv.s$span)


ftable(hrv.s$Language ~ span)
A <- cbind(ftable(hrv.s$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,17,by=2)
all.even = seq(2,17,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",17/2),rep("L",17/2+1))
C


hrv.s.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

hrv.s.multi.bar
hrv.s.multi.bar + ggtitle("Span distribution of crdo-HRV_SOURIS")

max(table(span))

hrv.s.multi.bar + ggtitle("Span distribution of crdo-HRV_SOURIS") + ylim(-50,280)


