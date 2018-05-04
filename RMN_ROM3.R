setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

rmn.r <- read_tsv("crdo-RMN-ROM3.tsv")

View(rmn.r)

summary(rmn.r)

rmn.r$span <- ""

init = 1
a = 1
rmn.r$span[a] = 1

while(a+1 <= nrow(rmn.r)) {
  if (rmn.r$Language[a] == rmn.r$Language[a+1]){
    rmn.r$span[a+1] <- rmn.r$span[a]} 
  else {rmn.r$span[a+1] <- as.integer(rmn.r$span[a])+1
  } 
  a= a+1
}


View(rmn.r)

span <- as.integer(rmn.r$span)

ftable(rmn.r$Language ~ span)
A <- cbind(ftable(rmn.r$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,215,by=2)
all.even = seq(2,215,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",215/2),rep("L",215/2+1))
C


rmn.r.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

rmn.r.multi.bar
rmn.r.multi.bar + ggtitle("Span distribution of crdo-RMN_ROM3")

max(table(span))

rmn.r.multi.bar + ggtitle("Span distribution of crdo-RMN_ROM3") + ylim(-20,40)