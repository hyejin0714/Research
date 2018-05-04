setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd.b <- read_tsv("crdo-MKD_BERGER.tsv")

View(mkd.b)

summary(mkd.b)

mkd.b$span <- ""

init = 1
a = 1
mkd.b$span[a] = 1

while(a+1 <= nrow(mkd.b)) {
  if (mkd.b$Language[a] == mkd.b$Language[a+1]){
    mkd.b$span[a+1] <- mkd.b$span[a]} 
  else {mkd.b$span[a+1] <- as.integer(mkd.b$span[a])+1
  } 
  a= a+1
}


View(mkd.b)

span <- as.integer(mkd.b$span)

ftable(mkd.b$Language ~ span)
A <- cbind(ftable(mkd.b$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,13,by=2)
all.even = seq(2,13,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",13/2),rep("L",13/2+1))
C


mkd.b.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

mkd.b.multi.bar
mkd.b.multi.bar + ggtitle("Span distribution of crdo-MKD_BERGER")

max(table(span))

mkd.b.multi.bar + ggtitle("Span distribution of crdo-MKD_BERGER") + ylim(-25,55)