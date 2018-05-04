setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

hsb.p <- read_tsv("crdo-HSB_PAQUES.tsv")

View(hsb.p)

summary(hsb.p)

hsb.p$span <- ""

init = 1
a = 1
hsb.p$span[a] = 1

while(a+1 <= nrow(hsb.p)) {
  if (hsb.p$Language[a] == hsb.p$Language[a+1]){
    hsb.p$span[a+1] <- hsb.p$span[a]} 
  else {hsb.p$span[a+1] <- as.integer(hsb.p$span[a])+1
  } 
  a= a+1
}


View(hsb.p)

span <- as.integer(hsb.p$span)

ftable(hsb.p$Language ~ span)
A <- cbind(ftable(hsb.p$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,12,by=2)
all.even = seq(2,12,by=2)

group.cl <- C[all.odd,]
group.l <- C[all.even,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.odd,all.even)
cl <- as.integer(C$CL[all.odd])
l<- as.integer(C$L[all.even])
C$y <- c(-cl,l)
C$group <- c(rep("CL",12/2),rep("L",12/2))
C


hsb.p.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous(breaks = seq(0,12,2)) + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

hsb.p.multi.bar
hsb.p.multi.bar + ggtitle("Span distribution of crdo-HSB_PAQUES")

max(table(span))

hsb.p.multi.bar + ggtitle("Span distribution of crdo-HSB_PAQUES") + ylim(-50,200)