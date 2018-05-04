setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

rmn.c <- read_tsv("crdo-RMN-COWARD.tsv")

View(rmn.c)

summary(rmn.c)

rmn.c$span <- ""

init = 1
a = 1
rmn.c$span[a] = 1

while(a+1 <= nrow(rmn.c)) {
  if (rmn.c$Language[a] == rmn.c$Language[a+1]){
    rmn.c$span[a+1] <- rmn.c$span[a]} 
  else {rmn.c$span[a+1] <- as.integer(rmn.c$span[a])+1
  } 
  a= a+1
}


View(rmn.c)

span <- as.integer(rmn.c$span)

ftable(rmn.c$Language ~ span)
A <- cbind(ftable(rmn.c$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,341,by=2)
all.even = seq(2,341,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",341/2),rep("L",341/2+1))
C


rmn.c.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

rmn.c.multi.bar
rmn.c.multi.bar + ggtitle("Span distribution of crdo-RMN_COWARD")

max(table(span))

rmn.c.multi.bar + ggtitle("Span distribution of crdo-RMN_COWARD") + ylim(-20,35)