# 3. DOMAČA NALOGA: KOLEKTIVNI MODEL TVEGANJA IN PANJERJEV ALGORITEM
# Finančni praktikum 2022/23
# Karolina Šavli

library(actuar)

# 1. naloga ####################################################################
# a)
vzorec1 <- scan(file = "vzorec1.txt")
histogram <- hist(vzorec1,
                  breaks = seq(2, 6, by = 0.25),
                  main = "Histogram odskodnin", 
                  xlab = "Visina odskodnine",
                  ylab = "Frequency",
                  col = "cadetblue3",
                  ylim = c(0, 80))
histogram

#-------------------------------------------------------------------------------

# b)
# podatkom primerna je Paretova porazdelitev (ukaz: pareto1; parametri: shape=alfa in min=x_m)
parametri <- mde(vzorec1, 
                 ppareto1, 
                 start = list("shape" = 1, "min" = 1), 
                 measure = "CvM")
# parametra:
shape <- parametri$estimate[1]
min <- parametri$estimate[2]

#-------------------------------------------------------------------------------

# c)
# histogram s paretovo porazdelitevijo:
histogram <- hist(vzorec1,
                  breaks = seq(2, 6, by=0.25),
                  main = "Histogram odskodnin", 
                  xlab = "Visina odskodnine",
                  ylab = "Density",
                  col = "cadetblue3",
                  probability = TRUE,
                  ylim = c(0,2))
curve(dpareto1(x, shape, min),
      col = "blue",
      add = TRUE,
      lwd = 1.5)
legend("topright", 
       legend = c("Paretova porazdelitev"), 
       col = "blue",
       cex = 0.7,
       lty = 1.5)

#grafična primerjava vzorčne in teoretične porazdelitvene funkcije:
plot(ecdf(vzorec1), 
     main = "Porazdelitvena funkcija odskodnin", 
     ylab = "Porazdelitvena funkcija", 
     xlab = "Visina odskodnine",
     cex = 0.4)
curve(ppareto1(x, shape, min), 
      col = "blue",
      add = TRUE,
      lwd = 2)
legend(3.5, 0.4, 
       legend = c("Empiricna porazdelitev", "Paretova porazdelitev"), 
       box.lty = 0, 
       col = c("black", "blue"),
       lty = 1:1, 
       cex = 0.8,
       pch = c(16, NA))

#-------------------------------------------------------------------------------

# d)
#upanje in disperzija Poissonove porazdelitve:
lambda <- 15
E_N <- lambda
Var_N <- lambda

#upanje in disperzija Paretove porazdelitve:
E_Y <- (shape * min) / (shape - 1)
Var_Y <- (min ^ 2 * shape) / ((shape - 1) ^ 2 * (shape - 2))

#upanje in disperzija kolektivne škode S:
E_S <- E_N * E_Y
E_S
Var_S <- Var_Y * E_N + E_Y * E_Y * Var_N
Var_S

# ==============================================================================
# ==============================================================================

# 2. naloga ####################################################################
# a)
h = 0.5
n = 50
diskretiziranje <- discretize(ppareto1(x, shape, min),
                  from = 0, 
                  to = n * h, 
                  step = h, 
                  method = "rounding")
diskretiziranje

#-------------------------------------------------------------------------------

# b)
# da bo lahko rumen graf pred črnim:
fun <- function(x) ppareto1(x,shape,min)
x <- seq(0, 25, 0.0001) 
plot(x, fun(x), 
     type = "l",
     ylab = "Porazdelitvena funkcija",
     ylim = c(0,1), 
     main = "Paretova porazdelitev",
     lwd = 1.5)
plot(stepfun(seq(0, (n - 1) * h, h), diffinv(diskretiziranje)), 
     pch = NA, 
     col  = "gold",
     lwd = 1.5,
     add = TRUE
)

#-------------------------------------------------------------------------------

# c)
S <- aggregateDist(method = "recursive", 
                   model.freq = "poisson",
                   model.sev = diskretiziranje,
                   lambda = lambda,
                   maxit = 1000000,
                   tol = 0.002, 
                   convolve = 0, 
                   x.scale = h)
S

#-------------------------------------------------------------------------------

# d)
E_S <- sum(knots(S) * diff(S))
E_S
E_S2 <- sum(knots(S) ^ 2 * diff(S))
Var_S <- E_S2 - E_S ^ 2
Var_S

# ==============================================================================
# ==============================================================================

# 3. naloga ####################################################################
# a)
#simulacija N:
simulacija_N <- rpois(10000, lambda)

#simulacija S:
simulacija_S <- c()
for (n in simulacija_N){
  simulacija_S <- c(simulacija_S, sum(rpareto1(n, shape, min) ))
}

#-------------------------------------------------------------------------------

# b)
E_simulacija_S <- mean(simulacija_S)
E_simulacija_S

Var_simulacija_S <- var(simulacija_S)
Var_simulacija_S

# primerjava:
# matematično upanje:
#       S:                     40.32363
#       Panjerjev algoritem:   39.89268
#       simulacija:            40.27231
# varianca:
#       S:                     123.5554
#       Panjerjev algoritem:   122.5199
#       simulacija:            122.1772
#
# ugotovitev: s simulacijo smo dobili točnejši rezultat za matematično upanje;
#             s Panjerjevim algoritmom pa smo dobili točnejši rezultat za varianco

#-------------------------------------------------------------------------------

# c)
plot(S,
     cex = 0.09)
plot(ecdf(simulacija_S), 
     add = TRUE, 
     col = "green")
legend(40, 0.3, 
       legend = c("Panjerjev algoritem","Monte Carlo simulacija"), 
       box.lty = 0, 
       col = c("black", "green"), 
       lty = 1:1,
       cex = 0.8)

