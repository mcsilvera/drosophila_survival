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

# Import male data and create survival object
LS_Machos <- read_excel("Lifespan.xlsx", sheet = "Machos")
surv_obj_machos <- Surv(LS_Machos$Días, LS_Machos$Estado)

# Perform Kaplan-Meier analysis
km_fit_machos <- survfit(surv_obj_machos ~ Genotipo, data = LS_Machos)

# Calculate log-rank test statistic for all possible comparisons between genotype groups in males

# Machos_prtpΔ1parkina vs Machos_w1118
test1 <- survdiff(surv_obj_machos ~ Genotipo, data = LS_Machos, subset = (Genotipo == "Machos_prtpΔ1parkina" | Genotipo == "Machos_w1118"))

# Machos_prtpΔ1parkina vs Machos_prtpΔ1
test2 <- survdiff(surv_obj_machos ~ Genotipo, data = LS_Machos, subset = (Genotipo == "Machos_prtpΔ1parkina" | Genotipo == "Machos_prtpΔ1"))

# Machos_prtpΔ1parkina vs Machos_parkina
test3 <- survdiff(surv_obj_machos ~ Genotipo, data = LS_Machos, subset = (Genotipo == "Machos_prtpΔ1parkina" | Genotipo == "Machos_parkina"))

# Machos_prtpΔ1 vs Machos_w1118
test4 <- survdiff(surv_obj_machos ~ Genotipo, data = LS_Machos, subset = (Genotipo == "Machos_prtpΔ1" | Genotipo == "Machos_w1118"))

# Machos_prtpΔ1 vs Machos_parkina
test5 <- survdiff(surv_obj_machos ~ Genotipo, data = LS_Machos, subset = (Genotipo == "Machos_prtpΔ1" | Genotipo == "Machos_parkina"))

# Machos_w1118 vs Machos_parkina
test6 <- survdiff(surv_obj_machos ~ Genotipo, data = LS_Machos, subset = (Genotipo == "Machos_w1118" | Genotipo == "Machos_parkina"))

# Extract results of each test
print(test1)
print(test2)
print(test3)
print(test4)
print(test5)
print(test6)

#Plot survival curves for all genotypes in males (survival probability)
ggsurvplot(km_fit_machos, data = LS_Machos, surv.median.line = "hv",
           xlab = "Días", ylab = "Probabilidad de sobrevida", palette = c("#FF0000", "#E89429", "#2BA803", "#000000"), font.x = c(16, "bold"), font.y = c(16, "bold"), font.tickslab = c(16), xlim = c(0, 105), break.x.by = 10, break.y.by = 0.1, axes.offset = FALSE, legend.title = "", legend = c(0.85, 0.9), legend.labs = c("Machos_parkina" = "park", "Machos_prtpΔ1" = "prtp", "Machos_prtpΔ1parkina" = "prtp-park",  "Machos_w1118" = "w"), font.legend = c(16, "italic"))

#Plot survival curves for all genotypes in males (survival percentage)
ggsurvplot(km_fit_machos, data = LS_Machos, surv.median.line = "hv",
           xlab = "Días", ylab = "Sobrevida", palette = c("#FF0000", "#E89429", "#2BA803", "#000000"), font.x = c(16, "bold"), font.y = c(16, "bold"), font.tickslab = c(16), xlim = c(0, 105), break.x.by = 10, break.y.by = 0.1, axes.offset = FALSE, surv.scale = "percent", legend.title = "", legend = c(0.85, 0.9), legend.labs = c("Machos_parkina" = "park", "Machos_prtpΔ1" = "prtp", "Machos_prtpΔ1parkina" = "prtp-park",  "Machos_w1118" = "w"), font.legend = c(16, "italic"))

