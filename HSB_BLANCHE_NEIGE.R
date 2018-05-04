setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

hsb.b <- read_tsv("crdo-HSB_BLANCHE_NEIGE.tsv")

View(hsb.b)

summary(hsb.b)

hsb.b$span <- ""

init = 1
a = 1
hsb.b$span[a] = 1

while(a+1 <= nrow(hsb.b)) {
  if (hsb.b$Language[a] == hsb.b$Language[a+1]){
    hsb.b$span[a+1] <- hsb.b$span[a]} 
  else {hsb.b$span[a+1] <- as.integer(hsb.b$span[a])+1
  } 
  a= a+1
}


View(hsb.b)

span <- as.integer(hsb.b$span)

ftable(hsb.b$Language ~ span)
A <- cbind(ftable(hsb.b$Language ~ span))
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


hsb.b.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

hsb.b.multi.bar
hsb.b.multi.bar + ggtitle("Span distribution of crdo-HSB_BLANCHE_NEIGE")

max(table(span))

hsb.b.multi.bar + ggtitle("Span distribution of crdo-HSB_BLANCHE_NEIGE")+ylim(-100,340) 
