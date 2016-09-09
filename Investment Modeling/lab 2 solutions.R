chipotle <-chipotle$Adj.Close[1:253]
chipotleyest <-chipotle$Adj.Close[2:254]
chipotledaily <- ((chipotle-chipotleyest)/chipotleyest)

mean(chipotledaily)
chipmean <- mean(chipotledaily)
chipstd <- sqrt(sum((chipotledaily-chipmean)^2))
chipsharpe <- (chipmean/chipstd)
chipdownside <- sqrt(sum((chipotledaily[which(chipotledaily < 0)]-chipmean))^2)
chipsortino <- (chipmean/chipdownside)

mcd <- mcdonalds
mcd <-mcdonalds$Adj.Close[1:253]
mcdyest <- mcdonalds$Adj.Close[2:254]
mcddaily <- ((mcd-mcdyest)/mcdyest)
mcdmean <- mean(mcddaily)
mcdstd <-sqrt(sum((mcddaily-mcdmean)^2))
mcdsharpe <- (mcdmean/mcdstd)
mcddownside <- sqrt(sum((mcddaily[which(mcddaily < 0)] - mcdmean))^2)
mcdsortino <- mcdmean/mcddownside

chipotlemin <- min(chipotledaily)
chipotlemax <- max(chipotledaily)
mcdonaldsmin <- min(mcddaily)
mcdonaldsmax <- max (mcddaily)
