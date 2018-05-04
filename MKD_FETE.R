setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

mkd.f <- read_tsv("crdo-MKD_FETE.tsv")

View(mkd.f)

summary(mkd.f)

mkd.f$span <- ""

init = 1
a = 1
mkd.f$span[a] = 1

while(a+1 <= nrow(mkd.f)) {
  if (mkd.f$Language[a] == mkd.f$Language[a+1]){
    mkd.f$span[a+1] <- mkd.f$span[a]} 
  else {mkd.f$span[a+1] <- as.integer(mkd.f$span[a])+1
  } 
  a= a+1
}


View(mkd.f)

span <- as.integer(mkd.f$span)

ftable(mkd.f$Language ~ span)
A <- cbind(ftable(mkd.f$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,72,by=2)
all.even = seq(2,72,by=2)

group.cl <- C[all.odd,]
group.l <- C[all.even,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.odd,all.even)
cl <- as.integer(C$CL[all.odd])
l<- as.integer(C$L[all.even])
C$y <- c(-cl,l)
C$group <- c(rep("CL",72/2),rep("L",72/2))
C


mkd.f.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

mkd.f.multi.bar
mkd.f.multi.bar + ggtitle("Span distribution of crdo-MKD_FETE")

max(table(span))

mkd.f.multi.bar + ggtitle("Span distribution of crdo-MKD_FETE") + ylim(-20,75)