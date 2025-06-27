install.packages("quantmod")
library(quantmod)
getSymbols("LGQM.DE", src = "yahoo", from = as.Date("2018-05-02"), to = as.Date("2025-04-02"), periodicity = "weekly")
getSymbols("CW8.PA", src = "yahoo", from = as.Date("2018-05-02"), to = as.Date("2025-04-02"), periodicity = "weekly")
getSymbols("CRP.PA", src = "yahoo", from = as.Date("2018-05-02"), to = as.Date("2025-04-02"), periodicity = "weekly")

LGQM.DE_normalized <- (Cl(LGQM.DE) / as.numeric(Cl(LGQM.DE)[1])) * 100
CW8.PA_normalized <- (Cl(CW8.PA) / as.numeric(Cl(CW8.PA)[1])) * 100
CRP.PA_normalized <- (Cl(CRP.PA) / as.numeric(Cl(CRP.PA)[1])) * 100


plot(CW8.PA_normalized, main = "Évolution Normalisée des ETFs",
     col = "blue", ylim = range(c(LGQM.DE_normalized, CW8.PA_normalized, CRP.PA_normalized)), ylab = "Valeur Normalisée")
lines(LGQM.DE_normalized, col = "red")
lines(CRP.PA_normalized, col = "green")
legend("topright", legend = c("LGQM.DE", "CW8.PA", "CRP.PA"), col = c("blue", "red", "green"), lty = 1, lwd = 2)






LGQM.DE_rendements <- weeklyReturn(LGQM.DE)
CW8.PA_rendements <- weeklyReturn(CW8.PA)
CRP.PA_rendements <- weeklyReturn(CRP.PA)

install.packages("e1071")
library(e1071)

calculate_statistics <- function(returns, name) {
  stats <- list(
    ETF = name,
    Mean = mean(returns),
    Median = median(returns),
    StdDev = sd(returns),
    Skewness = e1071::skewness(returns),
    Kurtosis_Excess = e1071::kurtosis(returns) - 3,
    Annualized_Return_Percent = mean(returns) * 52 * 100
  )
  return(stats)
}


LGQM.DE_stats <- calculate_statistics(LGQM.DE_rendements, "LGQM.DE")
CW8.PA_stats <- calculate_statistics(CW8.PA_rendements, "CW8.PA")
CRP.PA_stats <- calculate_statistics(CRP.PA_rendements, "CRP.PA")


stats_descriptives <- data.frame(rbind(LGQM.DE_stats, CW8.PA_stats, CRP.PA_stats))


print(stats_descriptives)
browseURL(stats_descriptives.url)

pdf("stats_descriptives.pdf")  
print(stats_descriptives)      


rendements <- data.frame(
  CW8.PA = as.numeric(CW8.PA_rendements),
  CRP.PA = as.numeric(CRP.PA_rendements),
  LGQM.DE = as.numeric(LGQM.DE_rendements)

)
matrice_correlation <- cor(rendements, use = "complete.obs")

print(matrice_correlation)

install.packages("gridExtra")
library(gridExtra)

pdf("matrice_correlation.pdf", width = 8, height = 6)

gridExtra::grid.table(round(matrice_correlation, 3))

dev.off()


par(mfrow = c(1, 3))

# ACF pour LGQM.DE
acf(as.numeric(LGQM.DE_rendements), main = "ACF - LGQM.DE", lag.max = 20)

# ACF pour CW8.PA
acf(as.numeric(CW8.PA_rendements), main = "ACF - CW8.PA", lag.max = 20)

# ACF pour CRP.PA
acf(as.numeric(CRP.PA_rendements), main = "ACF - CRP.PA", lag.max = 20)

par(mfrow = c(1, 1))
pdf("autocorrelation_rendements.pdf", width = 10, height = 6)


par(mfrow = c(1, 3))


acf(as.numeric(LGQM.DE_rendements), main = "ACF - LGQM.DE", lag.max = 20)
acf(as.numeric(CW8.PA_rendements), main = "ACF - CW8.PA", lag.max = 20)
acf(as.numeric(CRP.PA_rendements), main = "ACF - CRP.PA", lag.max = 20)

dev.off()
cat("Les graphiques ACF ont été enregistrés dans le fichier autocorrelation_rendements.pdf\n")


install.packages("rugarch")

library(rugarch)
library(xts)  
library(quantmod)

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(0,0), include.mean = TRUE), 
  distribution.model = "norm" 
)

garch_LGQM <- ugarchfit(spec = spec, data = as.numeric(LGQM.DE_rendements))
garch_CW8  <- ugarchfit(spec = spec, data = as.numeric(CW8.PA_rendements))
garch_CRP  <- ugarchfit(spec = spec, data = as.numeric(CRP.PA_rendements))


show(garch_LGQM)
show(garch_CW8)
show(garch_CRP)


cond_var_LGQM <- sigma(garch_LGQM)^2
cond_var_CW8  <- sigma(garch_CW8)^2
cond_var_CRP  <- sigma(garch_CRP)^2

dates_LGQM <- index(LGQM.DE_rendements)
dates_CW8  <- index(CW8.PA_rendements)
dates_CRP  <- index(CRP.PA_rendements)

par(mfrow = c(3, 2), mar = c(4, 4, 2, 1))  # Ajuste les marges

# --- LGQM.DE ---
plot(dates_LGQM, cond_var_LGQM, type = "l", col = "red",
     main = "LGQM.DE - Variance conditionnelle", xlab = "Date", ylab = "Variance conditionnelle")
plot(dates_LGQM, sqrt(cond_var_LGQM), type = "l", col = "blue",
     main = "LGQM.DE - Écart-type conditionnel", xlab = "Date", ylab = "Écart-type conditionnel")

# CW8.PA
plot(dates_CW8, cond_var_CW8, type = "l", col = "red",
     main = "CW8.PA - Variance conditionnelle", xlab = "Date", ylab = "Variance conditionnelle")
plot(dates_CW8, sqrt(cond_var_CW8), type = "l", col = "blue",
     main = "CW8.PA - Écart-type conditionnel", xlab = "Date", ylab = "Écart-type conditionnel")

# CRP.PA
plot(dates_CRP, cond_var_CRP, type = "l", col = "red",
     main = "CRP.PA - Variance conditionnelle", xlab = "Date", ylab = "Variance conditionnelle")
plot(dates_CRP, sqrt(cond_var_CRP), type = "l", col = "blue",
     main = "CRP.PA - Écart-type conditionnel", xlab = "Date", ylab = "Écart-type conditionnel")

par(mfrow = c(1, 1))





library(BEKKs)

data_bekk1 <- na.omit(cbind(CW8.PA = as.numeric(CW8.PA_rendements),
                           CRP.PA = as.numeric(CRP.PA_rendements)))
data_bekk1 <- as.matrix(data_bekk1)

spec <- bekk_spec()

bekk_model1 <- bekk_fit(spec, data_bekk1)

summary(bekk_model1)
plot(bekk_model1)

sigma_cw8 <- bekk_model1$sigma_t[[1]]
rho_cw8_crp <- bekk_model1$sigma_t[[2]]
sigma_crp <- bekk_model1$sigma_t[[3]]

covariance_conditionnelle <- sigma_cw8 * sigma_crp * rho_cw8_crp


dates <- index(na.omit(CW8.PA_rendements))
dates <- dates[1:length(covariance_conditionnelle)]

plot(dates, covariance_conditionnelle, type = "l",
     main = "Covariance conditionnelle CW8.PA ~ CRP.PA",
     xlab = "Date", ylab = "Covariance",
     col = "darkblue", lwd = 2)
abline(h = 0, col = "red", lty = 2)

sd_cw8 <- bekk_model1$sigma_t[[1]]
cor_cw8_crp <- bekk_model1$sigma_t[[2]]
sd_crp <- bekk_model1$sigma_t[[3]]

covariance_conditionnelle <- sd_cw8 * sd_crp * cor_cw8_crp

dates <- index(na.omit(CW8.PA_rendements))
dates <- dates[1:length(covariance_conditionnelle)]

col_curve <- "red"

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot(dates, sd_cw8, type = "l",
     main = "Conditional standard deviation of CW8.PA",
     xlab = "", ylab = "", col = col_curve, lwd = 2)

plot(dates, cor_cw8_crp, type = "l",
     main = "Conditional correlation of CW8.PA and CRP.PA",
     xlab = "", ylab = "", col = col_curve, lwd = 2)
abline(h = 0, col = "black", lty = 2)

plot(dates, sd_crp, type = "l",
     main = "Conditional standard deviation of CRP.PA",
     xlab = "", ylab = "", col = col_curve, lwd = 2)

plot(dates, covariance_conditionnelle, type = "l",
     main = "Conditional covariance of CW8.PA and CRP.PA",
     xlab = "", ylab = "", col = col_curve, lwd = 2)
abline(h = 0, col = "black", lty = 2)

par(mfrow = c(1, 1))  

-----------------------------------
library(BEKKs)
data_bekk2 <- na.omit(cbind(CW8.PA = as.numeric(CW8.PA_rendements),
                              LGQM.DE = as.numeric(LGQM.DE_rendements)))
data_bekk2 <- as.matrix(data_bekk2)

spec2 <- bekk_spec()

bekk_model2 <- bekk_fit(spec, data_bekk2)

summary(bekk_model2)
plot(bekk_model2)

sigma_cw8 <- bekk_model2$sigma_t[[1]]
rho_cw8_lgqm <- bekk_model2$sigma_t[[2]]
sigma_lgqm <- bekk_model2$sigma_t[[3]]

covariance_conditionnelle <- sigma_cw8 * sigma_lgqm * rho_cw8_lgqm


dates <- index(na.omit(CW8.PA_rendements))
dates <- dates[1:length(covariance_conditionnelle)]

plot(dates, covariance_conditionnelle, type = "l",
     main = "Covariance conditionnelle CW8.PA ~ LGQM.DE",
     xlab = "Date", ylab = "Covariance",
     col = "darkblue", lwd = 2)
abline(h = 0, col = "red", lty = 2)

sd_cw8 <- bekk_model2$sigma_t[[1]]
cor_cw8_lgqm <- bekk_model2$sigma_t[[2]]
sd_lgqm <- bekk_model2$sigma_t[[3]]

covariance_conditionnelle <- sd_cw8 * sd_lgqm * cor_cw8_lgqm

dates <- index(na.omit(CW8.PA_rendements))
dates <- dates[1:length(covariance_conditionnelle)]

col_curve <- "red"

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot(dates, sd_cw8, type = "l",
     main = "Conditional standard deviation of CW8.PA",
     xlab = "", ylab = "", col = col_curve, lwd = 2)

plot(dates, cor_cw8_lgqm, type = "l",
     main = "Conditional correlation of CW8.PA and LGQM.DE",
     xlab = "", ylab = "", col = col_curve, lwd = 2)
abline(h = 0, col = "black", lty = 2)

plot(dates, sd_lgqm, type = "l",
     main = "Conditional standard deviation of LGQM.DE",
     xlab = "", ylab = "", col = col_curve, lwd = 2)

plot(dates, covariance_conditionnelle, type = "l",
     main = "Conditional covariance of CW8.PA and LGQM.DE",
     xlab = "", ylab = "", col = col_curve, lwd = 2)
abline(h = 0, col = "black", lty = 2)

par(mfrow = c(1, 1))  



install.packages("vars")
library(vars)

data_var <- na.omit(cbind(CW8 = as.numeric(CW8.PA_rendements),
                          LGQM = as.numeric(LGQM.DE_rendements),
                          CRP  = as.numeric(CRP.PA_rendements)))
lag_selection <- VARselect(data_var, lag.max = 10, type = "const")
print(lag_selection$selection)
var_model <- VAR(data_var, p = 1, type = "const")  
summary(var_model)

par(mfrow = c(3, 1))
irf_LGQM <- irf(var_model,
                impulse = "LGQM",
                response = NULL,
                n.ahead = 12,
                boot = TRUE,
                ci = 0.95)
plot(irf_LGQM, main = "IRF : Choc sur LGQM (avec IC)")

irf_CW8 <- irf(var_model,
               impulse = "CW8",
               response = NULL,
               n.ahead = 12,
               boot = TRUE,
               ci = 0.95)
plot(irf_CW8, main = "IRF : Choc sur CW8 (avec IC)")

irf_CRP <- irf(var_model,
               impulse = "CRP",
               response = NULL,
               n.ahead = 12,
               boot = TRUE,
               ci = 0.95)
plot(irf_CRP, main = "IRF : Choc sur CRP (avec IC)")


par(mfrow = c(1, 1))





