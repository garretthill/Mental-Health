
dat=tsv.data

dat$hhinc



### recoding mental illness factor variables

dat$D_ASA12[dat$D_ASA12 == 5] <- 0 
dat$D_ASA12=as.numeric(dat$D_ASA12)

dat$D_PAT12[dat$D_PAT12 == 5] <- 0
dat$D_PAT12=as.numeric(dat$D_PAT12)

dat$D_ANO12[dat$D_ANO12 == 5] <- 0
dat$D_ANO12=as.numeric(dat$D_ANO12)

dat$D_MDE12[dat$D_MDE12 == 5] <- 0
dat$D_MDE12=as.numeric(dat$D_MDE12)

dat$D_GAD12[dat$D_GAD12 == 5] <- 0
dat$D_GAD12=as.numeric(dat$D_GAD12)

dat$D_DYS12[dat$D_DYS12 == 5] <- 0
dat$D_DYS12=as.numeric(dat$D_DYS12)

dat$D_AGO12[dat$D_AGO12 == 5] <- 0
dat$D_AGO12=as.numeric(dat$D_AGO12)

dat$D_HYP12[dat$D_HYP12 == 5] <- 0
dat$D_HYP12=as.numeric(dat$D_HYP12)

dat$D_IED12[dat$D_IED12 == 5] <- 0
dat$D_IED12=as.numeric(dat$D_IED12)

dat$D_BUL12[dat$D_BUL12 == 5] <- 0
dat$D_BUL12=as.numeric(dat$D_BUL12)

dat$D_SO12[dat$D_SO12 == 5] <- 0
dat$D_SO12=as.numeric(dat$D_SO12)

dat$D_SP12[dat$D_SP12 == 5] <- 0
dat$D_SP12=as.numeric(dat$D_SP12)

dat$D_MAN12[dat$D_MAN12 == 5] <- 0
dat$D_MAN12=as.numeric(dat$D_MAN12)










dat$mentalillness=dat$D_PAT12 + dat$D_SO12 + dat$D_MAN12 + dat$D_BUL12 + dat$D_ASA12 + dat$D_MDE12 + dat$D_ANO12 + dat$D_GAD12 + dat$D_DYS12 + dat$D_AGO12 + dat$D_HYP12 + dat$D_IED12 + dat$D_SP12 

dat$mentalillness

dat$micode=dat$mentalillness

dat$micode[dat$micode > 0] <- 1

dat$micode






dat$Sex
dat$Age
dat$ED4CAT
dat$ADULT
dat$wkstat3c
dat$RANCEST
dat$REGION
dat$HHSIZE


dat2=dat


xxx=which(is.na(dat2$hhinc))

dat2=dat2[-xxx,]


dat2$hhinc

dat2=dat2[dat2$ADULT==1,]

dat2=dat2[dat2$wkstat3==1,]

dat2=dat2[dat2$hhinc>0,]

dat2=dat2[dat2$hhinc<200000,]

dat2=dat2[dat2$hhinc>20000,]

dat2$hhinc

min(dat2$hhinc)
plot(dat2$hhinc)





fit <- lm(sqrt(hhinc) ~ Age  + as.factor(REGION) + micode + HHSIZE + ADULT + as.factor(ED4CAT) + as.factor(RANCEST), data=dat2)
summary(fit) 

hist(sqrt(dat2$hhinc))

dat2$pred=predict(fit,dat2)
dat2$money=dat2$pred^2

dat2$residuals=dat2$hhinc-dat2$money

plot(dat2$residuals)

plot(fit)


dat4=dat2[dat2$micode==1,]

dat5=dat4

dat5$micode=0

dat5$micode

dat4$prediction=predict(fit,dat4)
dat5$prediction=predict(fit,dat5)

dat4$money=dat4$prediction^2
dat5$money3=dat5$prediction^2

totti=dat5$money3-dat5$hhinc

totti

totti=na.omit(totti)

mean(totti)

median(totti)


max(totti)

min(totti)

xj=which(totti>50000)

tokki=totti*-1

tokki
xi=which(tokki>50000)

totti1=totti[-xi]

mean(totti1)
