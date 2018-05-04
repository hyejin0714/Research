setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd.p2 <- read_tsv("crdo-MKD_POIRES_PASSE.tsv")

View(mkd.p2)

summary(mkd.p2)

mkd.p2$span <- ""

init = 1
a = 1
mkd.p2$span[a] = 1

while(a+1 <= nrow(mkd.p2)) {
  if (mkd.p2$Language[a] == mkd.p2$Language[a+1]){
    mkd.p2$span[a+1] <- mkd.p2$span[a]} 
  else {mkd.p2$span[a+1] <- as.integer(mkd.p2$span[a])+1
  } 
  a= a+1
}


View(mkd.p2)

span <- as.integer(mkd.p2$span)

ftable(mkd.p2$Language ~ span)
A <- cbind(ftable(mkd.p2$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,7,by=2)
all.even = seq(2,7,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",7/2),rep("L",7/2+1))
C


mkd.p2.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

mkd.p2.multi.bar
mkd.p2.multi.bar + ggtitle("Span distribution of crdo-MKD_POIRES_PASSE")

max(table(span))

mkd.p2.multi.bar + ggtitle("Span distribution of crdo-MKD_POIRES_PASSE") + ylim(-20,85)