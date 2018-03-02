getwd()
setwd("C:/Users/hyejin/Desktop/INTERN/data")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd <- read_tsv("crdo-MKD.tsv")
summary(mkd)

mkd$span <- ""

init = 1
a = 1
mkd$span[a] = 1

while (a+1 <= nrow(mkd)){
  if (mkd$Language[a] == mkd$Language[a+1]){
    mkd$span[a+1] <- mkd$span[a]
  }else{
    mkd$span[a+1] <- as.integer(mkd$span[a]) + 1
  }
  a = a +1 
}


View(mkd)

span <- as.integer(mkd$span)


ftable(mkd$Language ~ span)
A <- cbind(ftable(mkd$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)


all.even = seq(2,755,by=2)
all.odd = seq(1,755,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl
group.l

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",754/2),rep("L",754/2+1))
C


mkd.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks=seq(1,755,by=50)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue"))

mkd.multi.bar
mkd.multi.bar + ggtitle("Span distribution of crdo-MKD") + ylim(-100,350)
