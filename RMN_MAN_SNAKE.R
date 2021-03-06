setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

rmn.m <- read_tsv("crdo-RMN-MAN_SNAKE.tsv")

View(rmn.m)

summary(rmn.m)

rmn.m$span <- ""

init = 1
a = 1
rmn.m$span[a] = 1

while(a+1 <= nrow(rmn.m)) {
  if (rmn.m$Language[a] == rmn.m$Language[a+1]){
    rmn.m$span[a+1] <- rmn.m$span[a]} 
  else {rmn.m$span[a+1] <- as.integer(rmn.m$span[a])+1
  } 
  a= a+1
}


View(rmn.m)

span <- as.integer(rmn.m$span)

ftable(rmn.m$Language ~ span)
A <- cbind(ftable(rmn.m$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,119,by=2)
all.even = seq(2,119,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",119/2),rep("L",119/2+1))
C


rmn.m.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

rmn.m.multi.bar
rmn.m.multi.bar + ggtitle("Span distribution of crdo-RMN_MAN_SNAKE")

max(table(span))

rmn.m.multi.bar + ggtitle("Span distribution of crdo-RMN_MAN_SNAKE") + ylim(-20,40)