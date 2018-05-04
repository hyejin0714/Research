setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd.p <- read_tsv("crdo-MKD_POIRES.tsv")

View(mkd.p)

summary(mkd.p)

mkd.p$span <- ""

init = 1
a = 1
mkd.p$span[a] = 1

while(a+1 <= nrow(mkd.p)) {
  if (mkd.p$Language[a] == mkd.p$Language[a+1]){
    mkd.p$span[a+1] <- mkd.p$span[a]} 
  else {mkd.p$span[a+1] <- as.integer(mkd.p$span[a])+1
  } 
  a= a+1
}


View(mkd.p)

span <- as.integer(mkd.p$span)

ftable(mkd.p$Language ~ span)
A <- cbind(ftable(mkd.p$Language ~ span))
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


mkd.p.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

mkd.p.multi.bar
mkd.p.multi.bar + ggtitle("Span distribution of crdo-MKD_POIRES")

max(table(span))

mkd.p.multi.bar + ggtitle("Span distribution of crdo-MKD_POIRES") + ylim(-10,65)