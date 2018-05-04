setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

hsb.com <- read_tsv("crdo-HSB_COMPETITION.tsv")

View(hsb.com)

summary(hsb.com)

hsb.com$span <- ""

init = 1
a = 1
hsb.com$span[a] = 1

while(a+1 <= nrow(hsb.com)) {
  if (hsb.com$Language[a] == hsb.com$Language[a+1]){
    hsb.com$span[a+1] <- hsb.com$span[a]} 
  else {hsb.com$span[a+1] <- as.integer(hsb.com$span[a])+1
  } 
  a= a+1
}


View(hsb.com)

span <- as.integer(hsb.com$span)

ftable(hsb.com$Language ~ span)
A <- cbind(ftable(hsb.com$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,59,by=2)
all.even = seq(2,59,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",59/2),rep("L",59/2+1))
C


hsb.com.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

hsb.com.multi.bar
hsb.com.multi.bar + ggtitle("Span distribution of crdo-HSB_COMPETITION")

max(table(span))

hsb.com.multi.bar + ggtitle("Span distribution of crdo-HSB_COMPETITION") + ylim(-25,90)