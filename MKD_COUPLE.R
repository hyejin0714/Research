setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd.cp <- read_tsv("crdo-MKD_COUPLE.tsv")

View(mkd.cp)

summary(mkd.cp)

mkd.cp$span <- ""

init = 1
a = 1
mkd.cp$span[a] = 1

while(a+1 <= nrow(mkd.cp)) {
  if (mkd.cp$Language[a] == mkd.cp$Language[a+1]){
    mkd.cp$span[a+1] <- mkd.cp$span[a]} 
  else {mkd.cp$span[a+1] <- as.integer(mkd.cp$span[a])+1
  } 
  a= a+1
}


View(mkd.cp)

span <- as.integer(mkd.cp$span)

ftable(mkd.cp$Language ~ span)
A <- cbind(ftable(mkd.cp$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,38,by=2)
all.even = seq(2,38,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",38/2),rep("L",38/2))
C


mkd.cp.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

mkd.cp.multi.bar
mkd.cp.multi.bar + ggtitle("Span distribution of crdo-MKD_COUPLE")

max(table(span))

mkd.cp.multi.bar + ggtitle("Span distribution of crdo-MKD_COUPLE") + ylim(-30,80)