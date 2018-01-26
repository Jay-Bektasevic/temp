ap <- AirPassengers
diff(ap), # lag = 2
diff(ap, lag = 2)

ap_diff <- as.numeric()

for (i in length(ap)){
    ap_diff[i-1] <- ap[i] - ap[(i-1)]
}
ap_diff == diff(ap)


# create an acf()   function manually

acf_ap <- acf(ap)

cent <- ap - mean(ap)
n <- length(cent)
ACF <- 0
z <- sum(cent * cent)/n

ACF[1] <- z/z

for (i in 1:24) {
    lag <- cent[-c(1:i)]
    clipped <- cent[1:length(lag)]
    ACF[i +1] <- (sum(clipped * lag)/n)/z
}

round(ACF, 6)[1:22] == round(as.vector(acf_ap$acf), 6)

#    test wheather the time series is stationary

require(tseries)
adf.test(rnorm(100)) # stationary b/c p-value is small

adf.test(diffinv(rnorm(100))) # not stationary data becasue the p-value is large 


