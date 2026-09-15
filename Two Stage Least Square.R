library(readxl)
data <- read_excel(file.choose())
summary(data[, 3:13])

# Pemodelan least square
m_ols <- lm(UP ~ PW + PO + PR + poly(Age,2)+RA+RU+Delay+Board+ROE+DER,
            data = data)
summary(m_ols)
ks.test(m_ols$residuals,"pnorm",mean(m_ols$residuals),sd(m_ols$residuals))

# Pemodelan two stage least square
library("ivreg")
m_iv <- ivreg(UP ~ PW + PO + PR|poly(Age,2)+RA+RU+Delay+Board+ROE+DER,
              data = data)
summary(m_iv)
ks.test(m_iv$residuals,"pnorm",mean(m_iv$residuals),sd(m_iv$residuals))

# Uji Heteroskedastisitas
library(lmtest)
bptest(m_ols) # Tidak terjadi heteroskedastisitas
bptest(m_iv) # Tidak terjadi heteroskedastisitas

# Uji Autokorelasi
library(lawstat)
runs.test(m_ols$residuals)
runs.test(m_iv$residuals)

# Uji Multikolinieritas
library(car)
vif_ols <- vif(m_ols)
vif_ols
vif_iv <- vif(m_iv)
vif_iv

library("modelsummary")
m_list <- list(OLS = m_ols, IV = m_iv)
msummary(m_list)

modelplot(m_list, coef_omit = "Intercept|experience")

