setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd.d <- read_tsv("crdo-MKD_DIMITRO.tsv")

View(mkd.d)

summary(mkd.d)

mkd.d$span <- ""

init = 1
a = 1
mkd.d$span[a] = 1

while(a+1 <= nrow(mkd.d)) {
  if (mkd.d$Language[a] == mkd.d$Language[a+1]){
    mkd.d$span[a+1] <- mkd.d$span[a]} 
  else {mkd.d$span[a+1] <- as.integer(mkd.d$span[a])+1
  } 
  a= a+1
}


View(mkd.d)

span <- as.integer(mkd.d$span)

ftable(mkd.d$Language ~ span)
A <- cbind(ftable(mkd.d$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,23,by=2)
all.even = seq(2,23,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",23/2),rep("L",23/2+1))
C


mkd.d.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

mkd.d.multi.bar
mkd.d.multi.bar + ggtitle("Span distribution of crdo-MKD_DIMITRO")

max(table(span))

mkd.d.multi.bar + ggtitle("Span distribution of crdo-MKD_DIMITRO") + ylim(-20,70)