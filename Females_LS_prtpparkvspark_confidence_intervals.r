# Install required packages
install.packages("readxl")
install.packages("openxlsx")
install.packages("survival")
install.packages("survminer")

# Load required packages
library(readxl)
library(openxlsx)
library(survival)
library(survminer)

# Set working directory manually

# Import female data and create survival object
LS_Hembras <- read_excel("Lifespan.xlsx", sheet = "HembrasCompImp")
surv_obj_hembras <- Surv(LS_Hembras$Días, LS_Hembras$Estado)

# Perform Kaplan-Meier analysis
km_fit_hembras <- survfit(surv_obj_hembras ~ Genotipo, data = LS_Hembras)

# Calculate log-rank test statistic for the comparison between genotype groups in females

# Hembras_prtpΔ1parkina vs Hembras_parkina
test <- survdiff(surv_obj_hembras ~ Genotipo, data = LS_Hembras, subset = (Genotipo == "Hembras_prtpΔ1parkina" | Genotipo == "Hembras_parkina"))

# Extract results of each test
print(test)

#Plot survival curves (survival probability)
ggsurvplot(km_fit_hembras, data = LS_Hembras, surv.median.line = "hv",
           xlab = "Días", ylab = "Probabilidad de sobrevida", palette = c("#FF0000", "#2BA803"), font.x = c(16, "bold"), font.y = c(16, "bold"), font.tickslab = c(16), xlim = c(0, 105), break.x.by = 10, break.y.by = 0.1, axes.offset = FALSE, legend.title = "", legend = c(0.85, 0.9), legend.labs = c("Hembras_parkina" = "park", "Hembras_prtpΔ1parkina" = "prtp-park"), font.legend = c(16, "italic"))

#Plot survival curves (survival percentage)
ggsurvplot(km_fit_hembras, data = LS_Hembras, surv.median.line = "hv",
           xlab = "Días", ylab = "Sobrevida", palette = c("#FF0000", "#2BA803"), font.x = c(16, "bold"), font.y = c(16, "bold"), font.tickslab = c(16), xlim = c(0, 105), break.x.by = 10, break.y.by = 0.1, axes.offset = FALSE, surv.scale = "percent", legend.title = "", legend = c(0.85, 0.9), legend.labs = c("Hembras_parkina" = "park", "Hembras_prtpΔ1parkina" = "prtp-park"), font.legend = c(16, "italic"))


#Plot survival curves with confidence intervals (survival probability)
ggsurvplot(km_fit_hembras, data = LS_Hembras, surv.median.line = "hv",
           xlab = "Días", ylab = "Probabilidad de sobrevida", palette = c("#FF0000", "#2BA803"), font.x = c(16, "bold"), font.y = c(16, "bold"), font.tickslab = c(16), xlim = c(0, 105), break.x.by = 10, break.y.by = 0.1, axes.offset = FALSE, legend.title = "", legend = c(0.85, 0.9), legend.labs = c("Hembras_parkina" = "park", "Hembras_prtpΔ1parkina" = "prtp-park"), font.legend = c(16, "italic"), conf.int = TRUE)

#Plot survival curves with confidence intervals (survival percentage)
ggsurvplot(km_fit_hembras, data = LS_Hembras, surv.median.line = "hv",
           xlab = "Días", ylab = "Sobrevida", palette = c("#FF0000", "#2BA803"), font.x = c(16, "bold"), font.y = c(16, "bold"), font.tickslab = c(16), xlim = c(0, 105), break.x.by = 10, break.y.by = 0.1, axes.offset = FALSE, surv.scale = "percent", legend.title = "", legend = c(0.85, 0.9), legend.labs = c("Hembras_parkina" = "park", "Hembras_prtpΔ1parkina" = "prtp-park"), font.legend = c(16, "italic"), conf.int = TRUE)

