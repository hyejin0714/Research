setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd.ch <- read_tsv("crdo-MKD_CHAMPS.tsv")

View(mkd.ch)

summary(mkd.ch)

mkd.ch$span <- ""

init = 1
a = 1
mkd.ch$span[a] = 1

while(a+1 <= nrow(mkd.ch)) {
  if (mkd.ch$Language[a] == mkd.ch$Language[a+1]){
    mkd.ch$span[a+1] <- mkd.ch$span[a]} 
  else {mkd.ch$span[a+1] <- as.integer(mkd.ch$span[a])+1
  } 
  a= a+1
}


View(mkd.ch)

span <- as.integer(mkd.ch$span)

ftable(mkd.ch$Language ~ span)
A <- cbind(ftable(mkd.ch$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,29,by=2)
all.even = seq(2,29,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",29/2),rep("L",29/2+1))
C


mkd.ch.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

mkd.ch.multi.bar
mkd.ch.multi.bar + ggtitle("Span distribution of crdo-MKD_CHAMPS")

max(table(span))

mkd.ch.multi.bar + ggtitle("Span distribution of crdo-MKD_CHAMPS") + ylim(-50,105)