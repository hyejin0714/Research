setwd("C:/Users/hyejin/Desktop/INTERN/data")
install.packages("readr")
install.packages("pillar")
library(readr)

install.packages(ggplot2)
library(ggplot2)

hsb <- read_tsv("crdo-HSB.tsv")
summary(hsb)

hsb$span <- ""

init = 1
a = 1
hsb$span[a] = 1

while (a+1 <= nrow(hsb)){
  if (hsb$Language[a] == hsb$Language[a+1]){
    hsb$span[a+1] <- hsb$span[a]
  }else{
    hsb$span[a+1] <- as.integer(hsb$span[a]) + 1
  }
  a = a +1 
}


View(hsb)

span <- as.integer(hsb$span)


ftable(hsb$Language ~ span)
A <- cbind(ftable(hsb$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C


all.odd = seq(1,381,by=2)
all.even = seq(2,381,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl
group.l


C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",380/2),rep("L",380/2+1))
C


hsb.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks=seq(1,381,by=50)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue"))

hsb.multi.bar
hsb.multi.bar + ggtitle("Span distribution of crdo-HSB") + ylim(-100,360)


