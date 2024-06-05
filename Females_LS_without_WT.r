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
LS_Hembras <- read_excel("Lifespan.xlsx", sheet = "Hembras")
surv_obj_hembras <- Surv(LS_Hembras$Días, LS_Hembras$Estado)

# Perform Kaplan-Meier analysis
km_fit_hembras <- survfit(surv_obj_hembras ~ Genotipo, data = LS_Hembras)

# Calculate log-rank test statistic for all possible comparisons between genotype groups in females

# Hembras_prtpΔ1parkina vs Hembras_w1118
test1 <- survdiff(surv_obj_hembras ~ Genotipo, data = LS_Hembras, subset = (Genotipo == "Hembras_prtpΔ1parkina" | Genotipo == "Hembras_w1118"))

# Hembras_prtpΔ1parkina vs Hembras_prtpΔ1
test2 <- survdiff(surv_obj_hembras ~ Genotipo, data = LS_Hembras, subset = (Genotipo == "Hembras_prtpΔ1parkina" | Genotipo == "Hembras_prtpΔ1"))

# Hembras_prtpΔ1parkina vs Hembras_parkina
test3 <- survdiff(surv_obj_hembras ~ Genotipo, data = LS_Hembras, subset = (Genotipo == "Hembras_prtpΔ1parkina" | Genotipo == "Hembras_parkina"))

# Hembras_prtpΔ1 vs Hembras_w1118
test4 <- survdiff(surv_obj_hembras ~ Genotipo, data = LS_Hembras, subset = (Genotipo == "Hembras_prtpΔ1" | Genotipo == "Hembras_w1118"))

# Hembras_prtpΔ1 vs Hembras_parkina
test5 <- survdiff(surv_obj_hembras ~ Genotipo, data = LS_Hembras, subset = (Genotipo == "Hembras_prtpΔ1" | Genotipo == "Hembras_parkina"))

# Hembras_w1118 vs Hembras_parkina
test6 <- survdiff(surv_obj_hembras ~ Genotipo, data = LS_Hembras, subset = (Genotipo == "Hembras_w1118" | Genotipo == "Hembras_parkina"))

# Extract results of each test
print(test1)
print(test2)
print(test3)
print(test4)
print(test5)
print(test6)

#Plot survival curves for all genotypes in females (survival probability)
ggsurvplot(km_fit_hembras, data = LS_Hembras, surv.median.line = "hv",
           xlab = "Días", ylab = "Probabilidad de sobrevida", palette = c("#FF0000", "#E89429", "#2BA803", "#000000"), font.x = c(16, "bold"), font.y = c(16, "bold"), font.tickslab = c(16), xlim = c(0, 105), break.x.by = 10, break.y.by = 0.1, axes.offset = FALSE, legend.title = "", legend = c(0.85, 0.9), legend.labs = c("Hembras_parkina" = "park", "Hembras_prtpΔ1" = "prtp", "Hembras_prtpΔ1parkina" = "prtp-park",  "Hembras_w1118" = "w"), font.legend = c(16, "italic"))

#Plot survival curves for all genotypes in females (survival percentage)
ggsurvplot(km_fit_hembras, data = LS_Hembras, surv.median.line = "hv",
           xlab = "Días", ylab = "Sobrevida", palette = c("#FF0000", "#E89429", "#2BA803", "#000000"), font.x = c(16, "bold"), font.y = c(16, "bold"), font.tickslab = c(16), xlim = c(0, 105), break.x.by = 10, break.y.by = 0.1, axes.offset = FALSE, surv.scale = "percent", legend.title = "", legend = c(0.85, 0.9), legend.labs = c("Hembras_parkina" = "park", "Hembras_prtp" = "prtpΔ1", "Hembras_prtpΔ1parkina" = "prtp-park",  "Hembras_w1118" = "w"), font.legend = c(16, "italic"))

