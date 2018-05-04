setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

hsb.chap <- read_tsv("crdo-HSB_CHAPERON_ROUGE.tsv")

View(hsb.chap)

summary(hsb.chap)

hsb.chap$span <- ""

init = 1
a = 1
hsb.chap$span[a] = 1

while(a+1 <= nrow(hsb.chap)) {
  if (hsb.chap$Language[a] == hsb.chap$Language[a+1]){
    hsb.chap$span[a+1] <- hsb.chap$span[a]} 
  else {hsb.chap$span[a+1] <- as.integer(hsb.chap$span[a])+1
  } 
  a= a+1
}


View(hsb.chap)

span <- as.integer(hsb.chap$span)

ftable(hsb.chap$Language ~ span)
A <- cbind(ftable(hsb.chap$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,19,by=2)
all.even = seq(2,19,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",19/2),rep("L",19/2+1))
C


hsb.chap.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

hsb.chap.multi.bar
hsb.chap.multi.bar + ggtitle("Span distribution of crdo-HSB_CHAPERON_ROUGE")

max(table(span))

hsb.chap.multi.bar + ggtitle("Span distribution of crdo-HSB_CHAPERON_ROUGE") + ylim(-50,200)
