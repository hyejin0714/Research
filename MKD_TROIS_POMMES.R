setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd.t <- read_tsv("crdo-MKD_TROIS_POMMES.tsv")

View(mkd.t)

summary(mkd.t)

mkd.t$span <- ""

init = 1
a = 1
mkd.t$span[a] = 1

while(a+1 <= nrow(mkd.t)) {
  if (mkd.t$Language[a] == mkd.t$Language[a+1]){
    mkd.t$span[a+1] <- mkd.t$span[a]} 
  else {mkd.t$span[a+1] <- as.integer(mkd.t$span[a])+1
  } 
  a= a+1
}


View(mkd.t)

span <- as.integer(mkd.t$span)

ftable(mkd.t$Language ~ span)
A <- cbind(ftable(mkd.t$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,21,by=2)
all.even = seq(2,21,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",21/2),rep("L",21/2+1))
C


mkd.t.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

mkd.t.multi.bar
mkd.t.multi.bar + ggtitle("Span distribution of crdo-MKD_TROIS_POMMES")

max(table(span))

mkd.t.multi.bar + ggtitle("Span distribution of crdo-MKD_TROIS_POMMES") + ylim(-50,330)