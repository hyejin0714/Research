setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd.mo <- read_tsv("crdo-MKD_MOISSON.tsv")

View(mkd.mo)

summary(mkd.mo)

mkd.mo$span <- ""

init = 1
a = 1
mkd.mo$span[a] = 1

while(a+1 <= nrow(mkd.mo)) {
  if (mkd.mo$Language[a] == mkd.mo$Language[a+1]){
    mkd.mo$span[a+1] <- mkd.mo$span[a]} 
  else {mkd.mo$span[a+1] <- as.integer(mkd.mo$span[a])+1
  } 
  a= a+1
}


View(mkd.mo)

span <- as.integer(mkd.mo$span)

ftable(mkd.mo$Language ~ span)
A <- cbind(ftable(mkd.mo$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,47,by=2)
all.even = seq(2,47,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",47/2),rep("L",47/2+1))
C


mkd.mo.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

mkd.mo.multi.bar
mkd.mo.multi.bar + ggtitle("Span distribution of crdo-MKD_MOISSON")

max(table(span))

mkd.mo.multi.bar + ggtitle("Span distribution of crdo-MKD_MOISSON") + ylim(-10,40)