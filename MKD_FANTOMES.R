setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd.fa <- read_tsv("crdo-MKD_FANTOMES.tsv")

View(mkd.fa)

summary(mkd.fa)

mkd.fa$span <- ""

init = 1
a = 1
mkd.fa$span[a] = 1

while(a+1 <= nrow(mkd.fa)) {
  if (mkd.fa$Language[a] == mkd.fa$Language[a+1]){
    mkd.fa$span[a+1] <- mkd.fa$span[a]} 
  else {mkd.fa$span[a+1] <- as.integer(mkd.fa$span[a])+1
  } 
  a= a+1
}


View(mkd.fa)

span <- as.integer(mkd.fa$span)

ftable(mkd.fa$Language ~ span)
A <- cbind(ftable(mkd.fa$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,43,by=2)
all.even = seq(2,43,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",43/2),rep("L",43/2+1))
C


mkd.fa.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

mkd.fa.multi.bar
mkd.fa.multi.bar + ggtitle("Span distribution of crdo-MKD_FANTOMES")

max(table(span))

mkd.fa.multi.bar + ggtitle("Span distribution of crdo-MKD_FANTOMES") + ylim(-20,75)