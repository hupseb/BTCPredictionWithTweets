install.packages("gtools")
library("gtools")

#Attempt to reproduce
var_1 <- ts(c(25.1,21.8,15.6,28.0,25.8,26.2,29.9,30.6,28.3,22.1,20.2,20.5,18.4,12.0,8.1,8.6,8.2,9.17,8.8,9.7,10.4))
var_2 <- ts(c(-13.1,-7.5,0.1,-3.4,-6.0,-4.6,-0.1,4.8,4.3,-1.1,-6.5,-10.0,-9.2,-7.8,-7.6,-7.1,-11.4,-14.2,-19.6,-22.9,-23.5))
running(var_1, var_2, fun=cor, width=5, by=1, allow.fewer=TRUE, align=c("right"), simplify=TRUE)

#Same thing but on changes   (use non-log approach as neg values)
chg_1 <- diff(var_1)/var_1[-length(var_1)] 
chg_2 <- diff(var_2)/var_2[-length(var_2)] 

running(chg_1, chg_2, fun=cor, width=5, by=1, allow.fewer=TRUE, align=c("right"), simplify=TRUE)

xts_var_1 <- xts(var_1, order.by = as.Date(var_1))
xts_var_2 <- xts(var_2, order.by = as.Date(var_2))
xts_var_1Per <- (xts_var_1/lag(xts_var_1,1) - 1)
xts_var_2Per <- (xts_var_2/lag(xts_var_2,1) - 1)

corrDf <- data.frame(xts_var_1Per, xts_var_2Per)
corrDf %>% ggpairs() 
corrDf <- data.frame(as.vector(xts_var_1Per), as.vector(xts_var_2Per))
corrDf %>% ggpairs() 
corrDf <- data.frame(as.vector(chg_1), as.vector(chg_2))
corrDf %>% ggpairs() 
ccf(var_1,var_2)
ccf(as.ts(xts_var_1),as.ts(xts_var_2))

ccf(chg_1,chg_2)
ccf(as.ts(xts_var_1Per[-1]),as.ts(xts_var_2Per[-1]))
