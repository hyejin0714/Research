setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd.s <- read_tsv("crdo-MKD_SOIE.tsv")

View(mkd.s)

summary(mkd.s)

mkd.s$span <- ""

init = 1
a = 1
mkd.s$span[a] = 1

while(a+1 <= nrow(mkd.s)) {
  if (mkd.s$Language[a] == mkd.s$Language[a+1]){
    mkd.s$span[a+1] <- mkd.s$span[a]} 
  else {mkd.s$span[a+1] <- as.integer(mkd.s$span[a])+1
  } 
  a= a+1
}


View(mkd.s)

span <- as.integer(mkd.s$span)

ftable(mkd.s$Language ~ span)
A <- cbind(ftable(mkd.s$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,63,by=2)
all.even = seq(2,63,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",63/2),rep("L",63/2+1))
C


mkd.s.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

mkd.s.multi.bar
mkd.s.multi.bar + ggtitle("Span distribution of crdo-MKD_SOIE")

max(table(span))

mkd.s.multi.bar + ggtitle("Span distribution of crdo-MKD_SOIE") + ylim(-20,80)