setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd.ct <- read_tsv("crdo-MKD_CONTE.tsv")

View(mkd.ct)

summary(mkd.ct)

mkd.ct$span <- ""

init = 1
a = 1
mkd.ct$span[a] = 1

while(a+1 <= nrow(mkd.ct)) {
  if (mkd.ct$Language[a] == mkd.ct$Language[a+1]){
    mkd.ct$span[a+1] <- mkd.ct$span[a]} 
  else {mkd.ct$span[a+1] <- as.integer(mkd.ct$span[a])+1
  } 
  a= a+1
}


View(mkd.ct)

span <- as.integer(mkd.ct$span)

ftable(mkd.ct$Language ~ span)
A <- cbind(ftable(mkd.ct$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,11,by=2)
all.even = seq(2,11,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",11/2),rep("L",11/2+1))
C


mkd.ct.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

mkd.ct.multi.bar
mkd.ct.multi.bar + ggtitle("Span distribution of crdo-MKD_CONTE")

max(table(span))

mkd.ct.multi.bar + ggtitle("Span distribution of crdo-MKD_CONTE") + ylim(-50,112)