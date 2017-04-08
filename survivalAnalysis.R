
library(survival)
library(dplyr)
#library(ggplot2)
library(GGally)

# # Read in annotation data
# clinData<-read.csv("/home/kartong/Code/CS4220_Bioinformatics_Project/annotations/BRCA.clin.merged.txt", sep="\t", header=F)
# clinDataClean<-as.data.frame(t(clinData[,2:1098]), stringsAsFactors = FALSE)
# colnames(clinDataClean)<-clinData$V1
# # 
# # 
# clinDataClean$patient.bcr_patient_uuid
# 
# as.numeric(clinDataClean$patient.days_to_death)
# 
# 
# adata.frame(
# clinDataClean$patient.days_to_new_tumor,
# clinDataClean$patient.days_to_initial_pathologic_diagnosis,
# clinDataClean$patient.days_to_last_followup,
# clinDataClean$patient.days_to_last_known_alive)
# 
# 
# 
# 
# # Columns with interesting clinical information
# "patient.breast_carcinoma_progesterone_receptor_status"
# "patient.breast_carcinoma_estrogen_receptor_status"
# "patient.her2_immunohistochemistry_level_result"
# 
# 
# patient.bcr_patient_uuid
#####################################################################


# Clinical data
clin_table_short = paste0("clin_table_short_",  "BRCA")
clin_table = read.delim("/home/kartong/Code/CS4220_Bioinformatics_Project/annotations/BRCA.merged_only_clinical_clin_format.txt", check.names = F, row.names = 1) %>% 
  t() %>% 
  as.data.frame()

days_to_new_tumor = apply(clin_table %>% select(matches("days_to_new_tumor_event_after_initial_treatment")), 1, function(x) { max(as.numeric(as.character(x)), na.rm = T) }) %>% ifelse(is.finite(.), ., "NA")
days_to_death = apply(clin_table %>% select(matches("days_to_death")), 1, function(x) { max(as.numeric(as.character(x)), na.rm = T) }) %>% ifelse(is.finite(.), ., "NA")
days_to_last_followup = apply(clin_table %>% select(matches("days_to_last_followup")), 1, function(x) { max(as.numeric(as.character(x)), na.rm = T) }) %>% ifelse(is.finite(.), ., "NA")
vital_status = apply(clin_table %>% select(matches("vital_status")), 1, function(x) { ifelse("dead" %in% x, "dead", ifelse("alive" %in% x, "alive", "NA")) } )
gender = clin_table$patient.gender
age = clin_table %>% select(matches("age_at_initial_pathologic_diagnosis")) %>% unlist() %>% as.character()
barcode = clin_table$patient.bcr_patient_barcode %>% toupper()

assign(clin_table_short, data.frame(barcode, days_to_new_tumor, days_to_death, days_to_last_followup, vital_status, gender, age) %>% 
         mutate(time = ifelse(!is.na(as.numeric(as.character(days_to_death))), as.numeric(as.character(days_to_death)), 
                              ifelse(!is.na(as.numeric(as.character(days_to_last_followup))), as.numeric(as.character(days_to_last_followup)), "NA")), 
                status = ifelse(as.character(vital_status) %in% c("alive", "dead"), as.character(vital_status), "NA"), 
                status = ifelse(as.character(status) == "alive", 0, ifelse(as.character(status) == "dead", 1, as.character(status)))))

clin_table_short_BRCA

#####################################################################


clin_table_short_BRCA$group<-c(rep(1,500), rep(0,597)) # for testing of groups
#clin_table_short_BRCA$group
survival_plot_df<-clin_table_short_BRCA

fit = survfit(Surv(as.numeric(survival_plot_df$time), as.numeric(survival_plot_df$status)) ~ survival_plot_df$group)
p4 = ggsurv(fit)
plot(fit)
plot(fit, col=c("red", "blue"))




# Plot survival
survival_plot_df = get(clin_table_short) %>% 
  mutate(exp_level = ifelse(barcode %in% high_barcodes, "high", ifelse(barcode %in% low_barcodes, "low", "NA"))) %>% 
  filter(exp_level != "NA")
fit = survfit(Surv(as.numeric(survival_plot_df$time), as.numeric(survival_plot_df$status)) ~ survival_plot_df$exp_level)
test = survdiff(Surv(as.numeric(survival_plot_df$time), as.numeric(survival_plot_df$status)) ~ survival_plot_df$exp_level)
p_value_sur = ifelse(is.na(test), next, (round(1 - pchisq(test$chisq, length(test$n) - 1), 3)))[[1]]

p4 = ggsurv(fit) + 
  labs(x = "Time (Days)") + 
  annotate("text", max(as.numeric(survival_plot_df$time), na.rm = T) / 2, 0.8, label = paste0("p-value: ", p_value_sur)) + 
  guides(linetype = FALSE) + 
  scale_colour_discrete(name = "exp_level") + 
  scale_y_continuous(name = "Overall Survival") + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())