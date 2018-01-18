library(survival)
crc <- read.csv("生存分析.csv")
#alive <- crc[crc[,3]=="alive",c(3,5)]
#dead <- crc[crc[,3]=="dead",c(3,4)]   crc<-crc[crc[,4]>30,]
m <- crc[crc[,3]=='dead' & crc[,4]>30,]
m[,4] <- m[,4]/30
n <- crc[crc[,3]=='alive',]
n[,5] <- n[,5]/30
crc <- rbind(m,n)
crc<-crc[-113,]

status <- matrix(nrow = nrow(crc), ncol = 2)
for(i in 1:nrow(crc)){
  if(crc[i,3]=="dead"){
    status[i,] <- c(1,crc[i,4])
  }
  if(crc[i,3]=="alive"){
    status[i,] <- c(0,crc[i,5])
  }
}
colnames(status) <- c("Status", "Time")
newcrc <- data.frame(status,crc[,c(1,2,6:16)])
#与初诊年龄关系
tmp <- newcrc[,c(1,2,5)]
tmp[tmp[,3]<62,3] <- 0
tmp[tmp[,3]>=62,3] <- 1
#tmp2<-tmp[!is.na(tmp[,2]),] 将第二列中有NA的一列去掉
coxph(Surv(Time,Status)~age_at_initial_pathologic_diagnosis,data = tmp)->coxph
survfit(Surv(Time,Status)~age_at_initial_pathologic_diagnosis,data = tmp) -> fit
plot(fit,col = c("green", "red"),main = "Surival Analysis of age",
     mark.time = FALSE,xlab="survival time(month)",ylab="Survival probability")
#0为红色
legend("topright",c("< 62",">= 62"),col = c("green","red"),lty = c(1,1),
       cex = 1,bty = "n")
#与性别关系
tmp <- newcrc[,c(1,2,7)]#female为红
coxph(Surv(Time,Status)~gender,data = tmp)->coxph
survfit(Surv(Time,Status)~gender,data = tmp) -> fit
plot(fit,col = c("green", "red"),main = "Surival Analysis of gender",
     mark.time = FALSE,xlab="survival time(month)",ylab="Survival probability")
#0为红色
legend("topright",c("female","male"),col = c("green","red"),lty = c(1,1),
       cex = 1,bty = "n")

#淋巴转移关系极显著
tmp <- newcrc[,c(1,2,12)]
coxph(Surv(Time,Status)~lymphatic_invasion,data = tmp)->coxph
survfit(Surv(Time,Status)~lymphatic_invasion,data = tmp) -> fit
plot(fit,col = c("green", "red"),main = "Surival Analysis of lymphatic_invasion",
     mark.time = FALSE,xlab="survival time(month)",ylab="Survival probability")
#0为红色
legend("topright",c("no","yes"),col = c("green","red"),lty = c(1,1),
       cex = 1,bty = "n")

#死亡率与lymphatic_invasion 淋巴转移关系极显著

#化疗关系极显著
tmp <- newcrc[,c(1,2,13)]
coxph(Surv(Time,Status)~radiation_therapy,data = tmp)->coxph
survfit(Surv(Time,Status)~radiation_therapy,data = tmp) -> fit
plot(fit,col = c("green", "red"),main = "Surival Analysis of radiation_therapy",
     mark.time = FALSE,xlab="survival time(month)",ylab="Survival probability")
#0为红色
legend("topright",c("no","yes"),col = c("green","red"),lty = c(1,1),
       cex = 1,bty = "n")

#血脉浸润
tmp <- newcrc[,c(1,2,14)]
 coxph(Surv(Time,Status)~venous_invasion,data = tmp)->coxph
 survfit(Surv(Time,Status)~venous_invasion,data = tmp) -> fit
 plot(fit,col = c("green", "red"),main = "Surival Analysis of venous_invasion",
      mark.time = FALSE,xlab="survival time(month)",ylab="Survival probability")
 #0为红色
 legend("topright",c("no","yes"),col = c("green","red"),lty = c(1,1),
        cex = 1,bty = "n")
 
 
 
 
 #淋巴转移关系极显著
 tmp <- newcrc[,c(1,2,15)]
 tmp[,3] <- sub("a$", "", tmp[,3], perl = T)
tmp[,3] <- sub("b$", "", tmp[,3], perl = T)
tmp[,3] <- sub("c$", "", tmp[,3], perl = T)
 
coxph(Surv(Time,Status)~pathologic_stage,data = tmp)->coxph
fit <- survfit(Surv(Time,Status)~pathologic_stage,data = tmp)
plot(fit,col = c("green", "red","blue","black"),main = "Surival Analysis of pathologic_stage",
     mark.time = FALSE,xlab="survival time(month)",ylab="Survival probability")
#0为红色
legend("topright",c("stage i","stage ii", "stage iii", "stage iv"),col = c("green","red"),lty = c(1,1),
       cex = 1,bty = "n")
 


############多因素的没法儿用 放弃吧~~~~

tmp <- newcrc[,c(1,2,5,12)]
tmp[tmp[,3]<62,3] <- 0
tmp[tmp[,3]>=62,3] <- 1
#tmp2<-tmp[!is.na(tmp[,2]),] 将第二列中有NA的一列去掉
coxph.model1 <- coxph(Surv(Time, Status) ~ age_at_initial_pathologic_diagnosis + 
                       lymphatic_invasion , data = tmp)
fit <- survfit(Surv(Time,Status) ~ age_at_initial_pathologic_diagnosis + 
                 lymphatic_invasion, data = tmp)
plot(fit,col = c("green", "red","blue","yellow"),main = "Surival Analysis of age",
     mark.time = FALSE,xlab="survival time(month)",ylab="Survival probability")
#0为红色
legend("topright",c("< 62, no","< 62, yes",">= 62, no",">=62, yes"),col = c("green", "red","blue","yellow"),lty = c(1,1),
       cex = 1,bty = "n")
#与性别关系 因素太多死的太少出错了