setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd.m <- read_tsv("crdo-MKD_MAIRE.tsv")

View(mkd.m)

summary(mkd.m)

mkd.m$span <- ""

init = 1
a = 1
mkd.m$span[a] = 1

while(a+1 <= nrow(mkd.m)) {
  if (mkd.m$Language[a] == mkd.m$Language[a+1]){
    mkd.m$span[a+1] <- mkd.m$span[a]} 
  else {mkd.m$span[a+1] <- as.integer(mkd.m$span[a])+1
  } 
  a= a+1
}


View(mkd.m)

span <- as.integer(mkd.m$span)

ftable(mkd.m$Language ~ span)
A <- cbind(ftable(mkd.m$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,75,by=2)
all.even = seq(2,75,by=2)

group.cl <- C[all.odd,]
group.l <- C[all.even,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.odd,all.even)
cl <- as.integer(C$CL[all.odd])
l<- as.integer(C$L[all.even])
C$y <- c(-cl,l)
C$group <- c(rep("CL",75/2+1),rep("L",75/2))
C


mkd.m.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

mkd.m.multi.bar
mkd.m.multi.bar + ggtitle("Span distribution of crdo-MKD_MAIRE")

max(table(span))

mkd.m.multi.bar + ggtitle("Span distribution of crdo-MKD_MAIRE") + ylim(-20,55)