setwd("C:/Users/hyejin/Desktop/INTERN/data")
install.packages("ggplots2")
library(ggplot2)

hrv <-read.csv("crdo-HRV.csv", header= TRUE)
View(hrv)
cl <- hrv[which(hrv$Language == "CL"),]


hrv$span <- ""
hrv$span[1:37] <- 1
hrv$span[38] <- 2
hrv$span[39:149] <- 3
hrv$span[150] <- 4
hrv$span[151:254] <- 5
hrv$span[255] <- 6
hrv$span[256:434] <- 7
hrv$span[435] <- 8
hrv$span[436:469] <- 9
hrv$span[470] <- 10
hrv$span[471:616] <- 11
hrv$span[617] <- 12
hrv$span[618:625] <- 13
hrv$span[626] <- 14
hrv$span[627:628] <- 15
hrv$span[629] <- 16
hrv$span[630:632] <- 17
hrv$span[633] <- 18
hrv$span[634:652] <- 19
hrv$span[653] <- 20
hrv$span[654:727] <- 21
hrv$span[728] <- 22
hrv$span[729:892] <- 23
hrv$span[893] <- 24
hrv$span[894:895] <- 25
hrv$span[896] <- 26
hrv$span[897:965] <- 27
hrv$span[966] <- 28
hrv$span[967:975] <- 29
hrv$span[976] <- 30
hrv$span[977:991] <- 31
hrv$span[992] <- 32
hrv$span[993:1054] <- 33
hrv$span[1055] <- 34
hrv$span[1056:1094] <- 35
hrv$span[1095:1096] <- 36
hrv$span[1097:1193] <- 37
hrv$span[1194] <- 38
hrv$span[1195:1403] <- 39
hrv$span[1404] <- 40
hrv$span[1405:1482] <- 41
hrv$span[1483] <- 42
hrv$span[1484:1586] <- 43
hrv$span[1587] <- 44
hrv$span[1588:1627] <- 45
hrv$span[1628] <- 46
hrv$span[1629:1646] <- 47
hrv$span[1647:1648] <- 48
hrv$span[1649:1926] <- 49
hrv$span[1927] <- 50
hrv$span[1928:2016] <- 51
hrv$span[2017] <- 52
hrv$span[2018:2166] <- 53
hrv$span[2167] <- 54
hrv$span[2168:2195] <- 55
hrv$span[2196] <- 56
hrv$span[2197:2211] <- 57
hrv$span[2212] <- 58
hrv$span[2213] <- 59
hrv$span[2214:2217] <- 60
hrv$span[2218:2234] <- 61
hrv$span[2235] <- 62
hrv$span[2236:2293] <- 63
hrv$span[2294] <- 64
hrv$span[2295:2309] <- 65
hrv$span[2310] <- 66
hrv$span[2311:2351] <- 67
hrv$span[2352] <- 68
hrv$span[2353:2357] <- 69
hrv$span[2358] <- 70
hrv$span[2359:2360] <- 71
hrv$span[2361] <- 72
hrv$span[2362:2376] <- 73
hrv$span[2377] <- 74
hrv$span[2378:2381] <- 75
hrv$span[2382] <- 76
hrv$span[2383:2430] <- 77
hrv$span[2431:2432] <- 78
hrv$span[2433] <- 79
hrv$span[2434] <- 80
hrv$span[2435:2439] <- 81
hrv$span[2440] <- 82
hrv$span[2441] <- 83
hrv$span[2442] <- 84
hrv$span[2443:2449] <- 85
hrv$span[2450] <- 86
hrv$span[2451:2464] <- 87
hrv$span[2465] <- 88
  
span <- as.integer(hrv$span)
barplot(table(span), xlab= "Span length", ylab="Span frequency", main = "Span distribution of crdo-HRv", col = c("blue","red"), border = c("blue", "red"), ylim = c(0,300))
legend("topright", legend = c("L", "CL"), fill = c("blue", "red"), cex=0.5)
barplot(table(span), xlab= "Span length", ylab="Span frequency", main = "Span distribution of crdo-HRV", col = c("blue","red"), border = c("blue", "red"), log="y")
legend("topright", legend = c("L", "CL"), fill = c("blue", "red"), cex=0.5)

a <- span[which (span %% 2 == 0)]
hrv[a,]