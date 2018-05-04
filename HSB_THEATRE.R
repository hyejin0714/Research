setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

hsb.t <- read_tsv("crdo-HSB_THEATRE.tsv")

View(hsb.t)

summary(hsb.t)

hsb.t$span <- ""

init = 1
a = 1
hsb.t$span[a] = 1

while(a+1 <= nrow(hsb.t)) {
  if (hsb.t$Language[a] == hsb.t$Language[a+1]){
    hsb.t$span[a+1] <- hsb.t$span[a]} 
  else {hsb.t$span[a+1] <- as.integer(hsb.t$span[a])+1
  } 
  a= a+1
}


View(hsb.t)

span <- as.integer(hsb.t$span)

ftable(hsb.t$Language ~ span)
A <- cbind(ftable(hsb.t$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,63,by=2)
all.even = seq(2,63,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",63/2),rep("L",63/2+1))
C


hsb.t.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

hsb.t.multi.bar
hsb.t.multi.bar + ggtitle("Span distribution of crdo-HSB_THEATRE")

max(table(span))

hsb.t.multi.bar + ggtitle("Span distribution of crdo-HSB_THEATRE") + ylim(-50,120)