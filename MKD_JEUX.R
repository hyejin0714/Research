setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd.j <- read_tsv("crdo-MKD_JEUX.tsv")

View(mkd.j)

summary(mkd.j)

mkd.j$span <- ""

init = 1
a = 1
mkd.j$span[a] = 1

while(a+1 <= nrow(mkd.j)) {
  if (mkd.j$Language[a] == mkd.j$Language[a+1]){
    mkd.j$span[a+1] <- mkd.j$span[a]} 
  else {mkd.j$span[a+1] <- as.integer(mkd.j$span[a])+1
  } 
  a= a+1
}


View(mkd.j)

span <- as.integer(mkd.j$span)

ftable(mkd.j$Language ~ span)
A <- cbind(ftable(mkd.j$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,41,by=2)
all.even = seq(2,41,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",41/2),rep("L",41/2+1))
C


mkd.j.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

mkd.j.multi.bar
mkd.j.multi.bar + ggtitle("Span distribution of crdo-MKD_JEUX")

max(table(span))

mkd.j.multi.bar + ggtitle("Span distribution of crdo-MKD_JEUX") + ylim(-20,95)