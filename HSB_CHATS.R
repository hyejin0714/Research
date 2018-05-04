setwd("C:/Users/hyejin/Desktop/INTERN/data/Pangloss")

install.packages("readr")
install.packages("pillar")

library(readr)

install.packages("ggplot2")
library(ggplot2)

hsb.chat <- read_tsv("crdo-HSB_CHATS.tsv")

View(hsb.chat)

summary(hsb.chat)

hsb.chat$span <- ""

init = 1
a = 1
hsb.chat$span[a] = 1

while(a+1 <= nrow(hsb.chat)) {
  if (hsb.chat$Language[a] == hsb.chat$Language[a+1]){
    hsb.chat$span[a+1] <- hsb.chat$span[a]} 
  else {hsb.chat$span[a+1] <- as.integer(hsb.chat$span[a])+1
  } 
  a= a+1
}


View(hsb.chat)

span <- as.integer(hsb.chat$span)

ftable(hsb.chat$Language ~ span)
A <- cbind(ftable(hsb.chat$Language ~ span))
C <- data.frame(A)
colnames(C) <- c("CL","L")
C

str(C)

all.odd = seq(1,60,by=2)
all.even = seq(2,60,by=2)

group.cl <- C[all.even,]
group.l <- C[all.odd,]
group.cl[,-2]
group.l[,-1]

C$span <- c(all.even,all.odd)
cl <- as.integer(C$CL[all.even])
l<- as.integer(C$L[all.odd])
C$y <- c(-cl,l)
C$group <- c(rep("CL",60/2),rep("L",60/2))
C


hsb.chat.multi.bar<- ggplot(C, aes(x= span, y= y , fill= group)) + 
  geom_bar(stat="identity", position="identity") + 
  scale_y_continuous(labels=abs)+
  scale_x_continuous() + 
  xlab("Span") + 
  ylab("Span Length") + 
  scale_fill_manual(values = c("red", "blue")) + theme_bw()

hsb.chat.multi.bar
hsb.chat.multi.bar + ggtitle("Span distribution of crdo-HSB_CHATS")

max(table(span))

hsb.chat.multi.bar + ggtitle("Span distribution of crdo-HSB_CHATS") + ylim(-20,90)
