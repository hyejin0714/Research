setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd.cf <- read_tsv("crdo-MKD_CONFRERIE.tsv")

View(mkd.cf)

summary(mkd.cf)

mkd.cf$span <- ""

init = 1
a = 1
mkd.cf$span[a] = 1

while(a+1 <= nrow(mkd.cf)) {
  if (mkd.cf$Language[a] == mkd.cf$Language[a+1]){
    mkd.cf$span[a+1] <- mkd.cf$span[a]} 
  else {mkd.cf$span[a+1] <- as.integer(mkd.cf$span[a])+1
  } 
  a= a+1
}


View(mkd.cf)

span <- as.integer(mkd.cf$span)

ftable(mkd.cf$Language ~ span)
A <- cbind(ftable(mkd.cf$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,115,by=2)
all.even = seq(2,115,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",115/2),rep("L",115/2+1))
C


mkd.cf.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

mkd.cf.multi.bar
mkd.cf.multi.bar + ggtitle("Span distribution of crdo-MKD_CONFRERIE")

max(table(span))

mkd.cf.multi.bar + ggtitle("Span distribution of crdo-MKD_CONFRERIE") + ylim(-50,150)