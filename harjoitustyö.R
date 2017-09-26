##TEHTÄVÄ 1
#a

yhteenveto <- function(x) {
  minmedmax <- c(min(x, na.rm = T), median(x, na.rm = T), max(x, na.rm = T))
  n <- length(x[is.na(x) == F])
  puuttuvia <- length(x[is.na(x) == T])
  keskiarvo <- mean(x, na.rm = T)
  keskihajonta <- sd(x, na.rm = T)
  return(list(minmedmax, n, puuttuvia, keskiarvo, keskihajonta))
}

#b
summamuuttuja <- function(nimet, data, max_puuttuvia = 1) {
  aineisto <- data[nimet]
  apply(aineisto, 1, function(x) x[length(is.na(x == T)) > max_puuttuvia] <- NA)
  v <- c(apply(aineisto, 1, function(x) mean(x, na.rm = T)))
  return(v)
}

#c
asty <- read.csv2("asiakastyytyvaisyys.csv", stringsAsFactors = F)
nimet <- c('K1A1', 'K1A2', 'K1A3')
asty$tyyt_kulj <- summamuuttuja(nimet, asty, 1)
yhteenveto(asty$tyyt_kulj)
boxplot(asty$tyyt_kulj, col = 'beige', main = "Asiakkaiden tyytyväisyys kuljettajiin")
abline(3.9336, 0, col = 'red')


#d
asty$tyyt_hsl <- summamuuttuja(c('K1A4', 'K1A5', 'K1A6', 'K2A2', 'K2A3', 'K2A4', 'K2A5', 'K2A6'), asty, 5)
yhteenveto(asty$tyyt_hsl)

##TEHTÄVÄ 2

#a
asty_osa <- asty[asty$LIIKENNEMUOTO == 3 | asty$LIIKENNEMUOTO == 4, ]
asty_osa$LIIKENNEMUOTO[asty_osa$LIIKENNEMUOTO == 3] <- "Metro"
asty_osa$LIIKENNEMUOTO[asty_osa$LIIKENNEMUOTO == 4] <- "Lahijuna"
asty_osa$LIIKENNEMUOTO <- as.factor(asty_osa$LIIKENNEMUOTO)
#b
table(asty_osa$K1A4, asty_osa$LIIKENNEMUOTO)
round(prop.table(table(asty_osa$K1A4, asty_osa$LIIKENNEMUOTO), 2), 2)
#c
chisq.test(table(asty_osa$LIIKENNEMUOTO,asty_osa$K1A4))

#d
t.test(asty_osa$K1A4[asty$LIIKENNEMUOTO == 2], mu = 4.3)
#h0 hylätään, 2.2e-16 < 0.05

#TEHTÄVÄ 3

#a
vuosi <- strtoi(strtrim(asty$PAIVAMAARA, 4))
asty$ika <- vuosi - asty$T7

#b
KeskiarvoJaVali <- function(x, confLevel) {
  n <- length(x) - length(x[is.na(x) == T])
  ka <- mean(x, na.rm = T)
  lower <- ka + (qt((1 - confLevel)/2, n - 1) * sd(x, na.rm = T)/sqrt(n))
  upper <- ka - (qt((1 - confLevel)/2, n - 1) * sd(x, na.rm = T)/sqrt(n))
  return(c(lower, ka, upper))
}

#c
asty$ikaluokka <- "<20"
asty$ikaluokka[asty$ika >= 20 & asty$ika < 30] <- "20-29"
asty$ikaluokka[asty$ika >= 30 & asty$ika < 40] <- "30-39"
asty$ikaluokka[asty$ika >= 40 & asty$ika < 50] <- "40-49"
asty$ikaluokka[asty$ika >= 50 & asty$ika < 60] <- "50-59"
asty$ikaluokka[asty$ika >= 60 & asty$ika < 70] <- "60-69"
asty$ikaluokka[asty$ika >= 70] <- "70<="
asty$ikaluokka[is.na(asty$ika)] <- NA

asty_juna <- asty[asty$LINJA == 'I' | asty$LINJA == 'P', ]
KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "<20"], 0.95)
KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "20-29"], 0.95)
KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "30-39"], 0.95)
KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "40-49"], 0.95)
KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "50-59"], 0.95)
KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "60-69"], 0.95)
KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "70<="], 0.95)

#d
plot(0, xlim = c(1, 7), ylim = c(1, 5), xaxt = "n", xlab = "Ikäryhmä", ylab = "Mielipide aikataulujen täsmällisyydestä")
axis(1, labels = as.graphicsAnnot(c("<20","20-29","30-39","40-49","50-59","60-69","70<=")), at = (1:7))
points(x = 1, mean(asty_juna$K1A4[asty_juna$ikaluokka == "<20"], na.rm = T))
points(x = 2, mean(asty_juna$K1A4[asty_juna$ikaluokka == "20-29"], na.rm = T))
points(x = 3, mean(asty_juna$K1A4[asty_juna$ikaluokka == "30-39"], na.rm = T))
points(x = 4, mean(asty_juna$K1A4[asty_juna$ikaluokka == "40-49"], na.rm = T))
points(x = 5, mean(asty_juna$K1A4[asty_juna$ikaluokka == "50-59"], na.rm = T))
points(x = 6, mean(asty_juna$K1A4[asty_juna$ikaluokka == "60-69"], na.rm = T))
points(x = 7, mean(asty_juna$K1A4[asty_juna$ikaluokka == "70<="], na.rm = T))

segments(1, y0 = KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "<20"], 0.95)[1], y1 = KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "<20"], 0.95)[3])
segments(2, y0 = KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "20-29"], 0.95)[1], y1 = KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "20-29"], 0.95)[3])
segments(3, y0 = KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "30-39"], 0.95)[1], y1 = KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "30-39"], 0.95)[3])
segments(4, y0 = KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "40-49"], 0.95)[1], y1 = KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "40-49"], 0.95)[3])
segments(5, y0 = KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "50-59"], 0.95)[1], y1 = KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "50-59"], 0.95)[3])
segments(6, y0 = KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "60-69"], 0.95)[1], y1 = KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "60-69"], 0.95)[3])
segments(7, y0 = KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "70<="], 0.95)[1], y1 = KeskiarvoJaVali(asty_juna$K1A4[asty_juna$ikaluokka == "70<="], 0.95)[3])

##TEHTÄVÄ 4

#a
yk <- read.csv2(file = "YK.csv", stringsAsFactors = F)

yk$LogBKT <- log(yk$BKT)

fit <- lm(yk$hedelmallisyys~yk$LogBKT)
summary(fit)

#b

plot(yk$LogBKT, yk$hedelmallisyys, xlim = c(5.5,15), ylim = c(1,8), xlab = "log - BKT", ylab = "hedelmällisyys")
points(yk[yk$alue == "Europe",]$LogBKT, yk[yk$alue == "Europe",]$hedelmallisyys, col = 'green')
points(yk[yk$alue == "Africa",]$LogBKT, yk[yk$alue == "Africa",]$hedelmallisyys, col = 'red')
points(yk[yk$alue == "Latin America",]$LogBKT, yk[yk$alue == "Latin America",]$hedelmallisyys, col = 'blue')
points(yk[yk$alue == "Asia",]$LogBKT, yk[yk$alue == "Asia",]$hedelmallisyys, col = 'yellow')
points(yk[yk$alue == "Caribbean",]$LogBKT, yk[yk$alue == "Caribbean",]$hedelmallisyys, col = 'brown')
points(yk[yk$alue == "Oceania",]$LogBKT, yk[yk$alue == "Oceania",]$hedelmallisyys, col = 'cyan')
legend(12,8, legend = c("Eurooppa", "Afrikka", "Lat. Amerikka", "Aasia", "Karibia", "Oseania"), col = c('green', 'red', 'blue', 'yellow', 'brown', 'cyan'), pch = 1)
curve(-0.70678*x + 8.71165, add = T, xlim = c(0,12))

yk$maa[yk$hedelmallisyys > 6.5]
text(yk$LogBKT[yk$maa == "Niger"], yk$hedelmallisyys[yk$maa == "Niger"], labels = "Niger", pos = 3)

yk$maa[yk$hedelmallisyys > 4.5 & yk$LogBKT > 10]
text(yk$LogBKT[yk$maa == "Equatorial Guinea"], yk$hedelmallisyys[yk$maa == "Equatorial Guinea"], labels = "Päiväntasaajan Guinea", pos = 3)

#c
fit2 <- lm(yk$hedelmallisyys~yk$LogBKT+yk$imevaiskuolleisuus)
summary(fit2)
confint(fit2, level = 0.99)

##TEHTÄVÄ 5

#a
simul <- function(n) {
  param.space <- runif(n)
  heads <- rbinom(n, 8, param.space)
  y <- param.space[heads == 2]
  return(y)
}

theta <- simul(100000)
hist(theta)
mean(theta)
median(theta)
sd(theta)

#b
runifdisc<-function(n, min=0, max=1) sample(min:max, n, replace=T)

simul2 <- function(n) {
  param.space <- c(0.1, 0.3, 0.4, 0.5, 0.7)
  kolikot <- runifdisc(n, min = 1, max = 5)
  p <- param.space[kolikot]
  heads <- rbinom(n, 8, p)
  havainnot <- kolikot[heads == 2]
  return(table(havainnot)/length(havainnot))
}

simul2(100000)



