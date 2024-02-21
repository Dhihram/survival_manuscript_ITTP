library(survival)
library(survminer)
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggfortify)
library(ranger)
library(readxl)
library(tidyr)
library(patchwork)
library(forestmodel)

#cleaning data
setwd("C:/Users/dhihr/Downloads")
NEWWW_Anlsiis_artikel2 <- read_excel("NEWWW_Anlsiis_artikel2(2).xls")
survival <- NEWWW_Anlsiis_artikel2
head(survival)
survival$No <- as.character(survival$No)
survival$TanggalambilswabTanggalTeri <- as.Date(survival$TanggalambilswabTanggalTeri)
survival$TglPulangIsolasiPerawatanMe <- as.Date(survival$TglPulangIsolasiPerawatanMe)
survival$TanggalHasilLabKeluarTangga <- as.Date(survival$TanggalHasilLabKeluarTangga)
survival$time <- difftime(survival$TglPulangIsolasiPerawatanMe, survival$TanggalHasilLabKeluarTangga, units = "days")
survival$time[survival$time < 0] <- 1
survival$status <- ifelse(survival$STATUSRawatSembuhMeninggal == "Sembuh",0,1)
survival$JenisKelamin_n <- ifelse(survival$JenisKelamin == "L",0,1)
survival$KelompokUmur_n <- ifelse(survival$KelompokUmur == "<=5",1,0)
survival$time <- as.numeric(survival$time)
survival$JenisKelamin <- ifelse(survival$JenisKelamin == "L", "Men", "Women")
table(survival$JenisKelamin)
survival$GangguanImmunologi <- ifelse(survival$GangguanImmunologi == 0, "Tidak", "Ya")
table(survival$GangguanImmunologi)
survival$Jantung <- ifelse(survival$Jantung == 0, "Tidak", "Ya")
survival$Date <- survival$TanggalHasilLabKeluarTangga
survival <- dplyr::rename(survival, ICU = ICUya1tidak0)

#tabel
table(survival$KelompokUmur, survival$status)
table(survival$JenisKelamin, survival$status)
table(survival$Pneumonia, survival$status)
table(survival$Jantung, survival$status)
table(survival$GagalGinjalKronis, survival$status)
table(survival$GangguanImmunologi, survival$status)
table(survival$ICU, survival$status)
table(survival$Keganasan, survival$status)
table(survival$Hipertensi, survival$status)

#kurva epid
agregat_harian_kasus <- survival %>% group_by(TanggalHasilLabKeluarTangga) %>% count()
survival <- survival %>% mutate(Sembuh = (STATUSRawatSembuhMeninggal == 'Sembuh'), Meninggal = (STATUSRawatSembuhMeninggal == 'Meninggal'))
agregat_outcome <- survival %>% group_by(TglPulangIsolasiPerawatanMe) %>% summarise(total = n(), Sembuh = sum(Sembuh), Meninggal = sum(Meninggal))
agregat_outcome <- rename(agregat_outcome, Date = TglPulangIsolasiPerawatanMe)
agregat_harian_kasus <- rename(agregat_harian_kasus, Date = TanggalHasilLabKeluarTangga)
agregat_harian_kasus <- rename(agregat_harian_kasus, Kasus = n)
agregat_harian_kasus <- left_join(agregat_harian_kasus, agregat_outcome, by = 'Date')
agregat_harian_kasus[is.na(agregat_harian_kasus)] <- 0
agregat_harian <- agregat_harian_kasus %>% pivot_longer(cols = c('Kasus', 'Sembuh', 'Meninggal'), names_to = 'Category',
                                                        values_to = 'case')
survival_kurva = subset(survival, Wilayah !="KEP. SERIBU")
ggplot(survival_kurva, aes(x = Date)) +geom_bar(color = "black", fill = "firebrick") + facet_wrap(~Wilayah, ncol = 2) + 
  theme_minimal() + theme(strip.text = element_text( size = 9, face = "bold" ))

psbb <- as.Date('2020-03-16')
psbb2 <- as.Date('2020-04-01')
eid <- as.Date('2020-05-23')
end <- as.Date(last(agregat_harian$Date))
p <- ggplot(agregat_harian, aes(x = Date, y = case, fill = Category)) + geom_bar(stat = "identity", color = 'black') + 
  scale_fill_manual(values = c("lightcyan", "red", "lightblue")) + theme_minimal()
p +  geom_vline(xintercept=psbb, linetype="dashed", color = "blue", size = 1.5) +
  annotate(geom="text", x= psbb2, y=150, label = "School Closure: 16 Mar 2020", color = "blue")



#ujicoba model univariat dan kaplan-meier
sfit_jantung <- survfit(Surv(time, status)~Jantung, data=survival)
heart <- ggsurvplot(sfit_jantung, pval=TRUE, conf.int = TRUE, surv.median.line = "hv", risk.table = TRUE,  
           legend.labs = paste("Heart Diseases", as.factor(c('No','Yes'))), 
           legend = 'none', title = "Heart Diseases", ggtheme = theme_bw())
heart
sfit_gagalginjalkronis <- survfit(Surv(time, status)~GagalGinjalKronis, data=survival)
crd <- ggsurvplot(sfit_gagalginjalkronis, pval=TRUE, conf.int = TRUE, surv.median.line = "hv", risk.table = TRUE, 
           legend.labs = paste("Chronic Renal Disease", as.factor(c('No','Yes'))), 
           legend = 'none', title = "Chronic Renal Disease", ggtheme = theme_bw())
crd
sfit_kelompokumur <- survfit(Surv(time, status)~KelompokUmur, data=survival)
age <- ggsurvplot(sfit_kelompokumur, pval=TRUE, conf.int = TRUE, surv.median.line = "hv",
           risk.table = TRUE, 
           legend.labs = paste("Age Groups", as.factor(c('<6','6-19'))), 
           legend = 'none', title = "Age Groups", ggtheme = theme_bw())
age
sfit_gender <- survfit(Surv(time, status)~JenisKelamin, data=survival)
gender <- ggsurvplot(sfit_kelompokumur, pval=TRUE, conf.int = TRUE, surv.median.line = "hv",
                  risk.table = TRUE, 
                  legend.labs = paste("Gender", as.factor(c('Male','Female'))), 
                  legend = 'none', title = "Gender", ggtheme = theme_bw())
sfit_gangguanimunologi <- survfit(Surv(time, status)~GangguanImmunologi, data=survival)
Immun <- ggsurvplot(sfit_gangguanimunologi, pval=TRUE, conf.int = TRUE, surv.median.line = "hv",
           risk.table = TRUE, 
           legend.labs = paste("Immunocompromised", as.factor(c('No','Yes'))), 
           legend = 'none', title = "Immunocompromised", ggtheme = theme_bw())
Immun
sfit_hipertensi <- survfit(Surv(time, status)~Hipertensi, data=survival)
hyp <- ggsurvplot(sfit_hipertensi, pval=TRUE, conf.int = TRUE, surv.median.line = "hv",
           risk.table = TRUE, 
           legend.labs = paste("Hypertension", as.factor(c('No','Yes'))), 
           legend = 'none', title = "Hypertension", ggtheme = theme_bw())
hyp
sfit_pneumonia <- survfit(Surv(time, status)~Pneumonia, data=survival)
pneu <- ggsurvplot(sfit_pneumonia, pval=TRUE, conf.int = TRUE, surv.median.line = "hv",
           risk.table = TRUE, 
           legend.labs = paste("Pneumonia", as.factor(c('No','Yes'))), 
           legend = 'none', title = "Pneumonia", ggtheme = theme_bw())
pneu
sfit_ICU <- survfit(Surv(time, status)~ICU, data=survival)
ICU <- ggsurvplot(sfit_ICU, pval=TRUE, conf.int = TRUE, surv.median.line = "hv",
           risk.table = TRUE, 
           legend.labs = paste("ICU", as.factor(c('No','Yes'))), 
           legend = 'none', title = "ICU", ggtheme = theme_bw())
ICU


#univariat
covariates <- c("JenisKelamin_n", "ICU",  "Obesitas_n", "Pneumonia", "Jantung_n", "GagalGinjalKronis_n", "Keganasan_n", "GangguanImmunologi_n", "KelompokUmur_n", "Hipertensi_n")
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(time, status)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = survival)})
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
res2 <- as.data.frame(res)
res2

#multivariat
survival$Pneumonia <- ifelse(survival$Pneumonia_n == 0, 'No', 'Yes')
survival$GangguanImmunologi <- ifelse(survival$GangguanImmunologi_n == 0 , 'No', 'Yes')
survival$KelompokUmur <- ifelse(survival$KelompokUmur_n == 0, '6-19', '<6')
survival$Pneumonia <- factor(survival$Pneumonia, levels = c("No", "Yes"))
survival$GangguanImmunologi <- factor(survival$GangguanImmunologi, levels = c("No", "Yes"))
survival$KelompokUmur <- factor(survival$KelompokUmur, levels = c('6-19', '<6'))
colnames(x = survival)[colnames(x = survival)== "Pneumonia"] <- 'Pneumonia'
colnames(x = survival)[colnames(x = survival) == "GangguanImmunologi"] <- 'Immunocompromised'
colnames(x = survival)[colnames(x = survival) == "KelompokUmur"] <- 'Age Group'

res.cox <- coxph(Surv(time, status) ~Pneumonia + Immunocompromised + `Age Group`, data =  survival)
print(forest_model(res.cox)) 
#uji asumsi
test.ph <- cox.zph(res.cox)
test.ph
ggcoxzph(test.ph)
ggcoxdiagnostics(res.cox, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_minimal())

library(rio)
library(openxlsx)
res2 <- rownames_to_column(res2, "variabel")
write.xlsx(res2, "C:/Users/dhihr/Documents/survival.xlsx")
table(survival$Hipertensi)
table(survival$STATUSRawatSembuhMeninggal_n)
