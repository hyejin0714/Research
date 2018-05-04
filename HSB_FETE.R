setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

hsb.f <- read_tsv("crdo-HSB_FETE.tsv")

View(hsb.f)

summary(hsb.f)

hsb.f$span <- ""

init = 1
a = 1
hsb.f$span[a] = 1

while(a+1 <= nrow(hsb.f)) {
  if (hsb.f$Language[a] == hsb.f$Language[a+1]){
    hsb.f$span[a+1] <- hsb.f$span[a]} 
  else {hsb.f$span[a+1] <- as.integer(hsb.f$span[a])+1
  } 
  a= a+1
}


View(hsb.f)

span <- as.integer(hsb.f$span)

ftable(hsb.f$Language ~ span)
A <- cbind(ftable(hsb.f$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,57,by=2)
all.even = seq(2,57,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",57/2),rep("L",57/2+1))
C


hsb.f.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

hsb.f.multi.bar
hsb.f.multi.bar + ggtitle("Span distribution of crdo-HSB_FETE")

max(table(span))

hsb.f.multi.bar + ggtitle("Span distribution of crdo-HSB_FETE") + ylim(-50,205)