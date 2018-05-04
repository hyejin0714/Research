setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

hsb.m<- read_tsv("crdo-HSB_MARIAGE.tsv")

View(hsb.m)

summary(hsb.m)

hsb.m$span <- ""

init = 1
a = 1
hsb.m$span[a] = 1

while(a+1 <= nrow(hsb.m)) {
  if (hsb.m$Language[a] == hsb.m$Language[a+1]){
    hsb.m$span[a+1] <- hsb.m$span[a]} 
  else {hsb.m$span[a+1] <- as.integer(hsb.m$span[a])+1
  } 
  a= a+1
}


View(hsb.m)

span <- as.integer(hsb.m$span)

ftable(hsb.m$Language ~ span)
A <- cbind(ftable(hsb.m$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,108,by=2)
all.even = seq(2,108,by=2)

group.cl <- C[all.odd,]
group.l <- C[all.even,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.odd,all.even)
cl <- as.integer(C$CL[all.odd])
l<- as.integer(C$L[all.even])
C$y <- c(-cl,l)
C$group <- c(rep("CL",108/2),rep("L",108/2))
C


hsb.m.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

hsb.m.multi.bar
hsb.m.multi.bar + ggtitle("Span distribution of crdo-HSB_MARIAGE")

max(table(span))

hsb.m.multi.bar + ggtitle("Span distribution of crdo-HSB_MARIAGE") + ylim(-25,125)