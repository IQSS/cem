### R code from vignette source 'cem.Rnw'

###################################################
### code chunk number 1: cem.Rnw:355-357
###################################################
options("digits"=4)
options("width"=80)


###################################################
### code chunk number 2: cem.Rnw:359-361
###################################################
require(cem)
data(LeLonde)


###################################################
### code chunk number 3: cem.Rnw:366-367
###################################################
Le <- data.frame(na.omit(LeLonde))


###################################################
### code chunk number 4: cem.Rnw:370-374
###################################################
tr <- which(Le$treated==1)
ct <- which(Le$treated==0)
ntr <- length(tr)
nct <- length(ct)


###################################################
### code chunk number 5: cem.Rnw:380-381
###################################################
mean(Le$re78[tr]) - mean(Le$re78[ct])


###################################################
### code chunk number 6: cem.Rnw:388-390
###################################################
vars <- c("age", "education", "black", "married", "nodegree", "re74",
"re75", "hispanic", "u74", "u75","q1")


###################################################
### code chunk number 7: cem.Rnw:394-395
###################################################
L1 <- L1.meas(Le$treated, Le[vars])$L1 


###################################################
### code chunk number 8: cem.Rnw:402-403
###################################################
imbalance(group=Le$treated, data=Le[vars])


###################################################
### code chunk number 9: cem.Rnw:478-479
###################################################
mat <- cem(treatment = "treated", data = Le, drop = "re78",keep.all=TRUE)


###################################################
### code chunk number 10: cem.Rnw:494-495
###################################################
mat


###################################################
### code chunk number 11: cem.Rnw:538-539
###################################################
levels(Le$q1)


###################################################
### code chunk number 12: cem.Rnw:543-544
###################################################
q1.grp <- list(c("strongly agree", "agree"), c("neutral","no opinion"), c("strongly disagree","disagree"))


###################################################
### code chunk number 13: cem.Rnw:566-567
###################################################
table(Le$education)


###################################################
### code chunk number 14: cem.Rnw:570-571
###################################################
educut <- c(0, 6.5, 8.5, 12.5, 17)


###################################################
### code chunk number 15: cem.Rnw:575-578
###################################################
mat1 <- cem(treatment = "treated", data = Le, drop = "re78", 
cutpoints = list(education=educut), grouping=list(q1=q1.grp))
mat1


###################################################
### code chunk number 16: cem.Rnw:586-587
###################################################
mat$breaks$education


###################################################
### code chunk number 17: cem.Rnw:590-591
###################################################
mat1$breaks$education


###################################################
### code chunk number 18: cem.Rnw:643-646
###################################################
cem("treated", Le, cutpoints = list(age=10), drop="re78", grouping=list(q1=q1.grp))
cem("treated", Le, cutpoints = list(age=6), drop="re78", grouping=list(q1=q1.grp))
cem("treated", Le, cutpoints = list(age=3), drop="re78", grouping=list(q1=q1.grp))


###################################################
### code chunk number 19: cem.Rnw:661-662
###################################################
tab <- relax.cem(mat, Le, depth=1, perc=0.3) 


###################################################
### code chunk number 20: cem.Rnw:666-669
###################################################
pdf("coarsen1.pdf", width=9, height=6, pointsize=10)
plot(tab,perc=0.3)
invisible(dev.off())


###################################################
### code chunk number 21: cem.Rnw:711-712
###################################################
plot(tab, group="1", perc=0.35,unique=TRUE)


###################################################
### code chunk number 22: cem.Rnw:716-719
###################################################
pdf("coarsen2.pdf", width=9, height=6, pointsize=10)
plot(tab, group="1", perc=0.35,unique=TRUE)
invisible(dev.off())


###################################################
### code chunk number 23: cem.Rnw:751-754
###################################################
mat <- cem(treatment="treated",data=Le, drop="re78")
mat
mat$k2k


###################################################
### code chunk number 24: cem.Rnw:758-761
###################################################
mat2 <- k2k(mat, Le, "euclidean", 1)
mat2
mat2$k2k


###################################################
### code chunk number 25: cem.Rnw:773-777
###################################################
data(LL)
mat <- cem(treatment="treated", data=LL, drop="re78")
est <- att(mat, re78 ~ treated, data = LL)
est


###################################################
### code chunk number 26: cem.Rnw:798-800
###################################################
est2 <- att(mat, re78 ~ treated + re74, data = LL)
est2


###################################################
### code chunk number 27: cem.Rnw:806-807
###################################################
att(mat, re78 ~ treated + re74 , data = LL, model="linear")


###################################################
### code chunk number 28: cem.Rnw:810-811
###################################################
att(mat, re78 ~ treated + re74 , data = LL, model="linear-RE")


###################################################
### code chunk number 29: cem.Rnw:815-816
###################################################
att(mat, re78 ~ treated + re74 , data = LL, model="forest")


###################################################
### code chunk number 30: cem.Rnw:821-824
###################################################
att(mat, re78 ~ treated + re74 , data = LL, model="linear", extra=TRUE)
att(mat, re78 ~ treated + re74 , data = LL, model="linear-RE", extra=TRUE)
att(mat, re78 ~ treated + re74 , data = LL, model="rf", extra=TRUE)


###################################################
### code chunk number 31: cem.Rnw:827-830
###################################################
est3 <- att(mat, re78 ~ treated + re74 , data = LL)
est3
plot(est3, mat, LL, vars=c("education", "age", "re74", "re75"))


###################################################
### code chunk number 32: cem.Rnw:834-838
###################################################
pdf("teff.pdf", width=9, height=6, pointsize=10)
est3 <- att(mat, re78 ~ treated + re74 + re75, data = LL)
plot(est3, mat, LL, vars=c("education", "age", "re74", "re75"))
invisible(dev.off())


###################################################
### code chunk number 33: cem.Rnw:877-879
###################################################
mat3 <- cem("treated", LeLonde, drop="re78", cutpoints = mat$breaks, grouping=list(q1=q1.grp))
mat3


###################################################
### code chunk number 34: cem.Rnw:882-884
###################################################
mat4 <- cem("treated", Le, drop="re78", cutpoints = mat$breaks, grouping=list(q1=q1.grp))
mat4


###################################################
### code chunk number 35: cem.Rnw:902-903
###################################################
summary(LeLonde)


###################################################
### code chunk number 36: cem.Rnw:907-912
###################################################
require(Amelia)
set.seed(123)
imputed <- amelia(LeLonde,noms=c("black","hispanic","treated","married","nodegree",
"u74","u75","q1"))
imputed <- imputed$imputations[1:5]


###################################################
### code chunk number 37: cem.Rnw:929-931
###################################################
mat2 <- cem("treated", datalist=imputed, drop="re78", data=LeLonde, grouping=list(q1=q1.grp))
mat2


###################################################
### code chunk number 38: cem.Rnw:938-940
###################################################
out <- att(mat2, re78 ~ treated, data=imputed)
out


###################################################
### code chunk number 39: cem.Rnw:946-953
###################################################
data(LL)

# cem match: automatic bin choice
mat <- cem(data=LL, drop="re78")

# we want a set of paired units
psample <- pair(mat, data=LL)


###################################################
### code chunk number 40: cem.Rnw:956-957
###################################################
table(psample$paired)


###################################################
### code chunk number 41: cem.Rnw:960-961
###################################################
psample$paired[1:100]


###################################################
### code chunk number 42: cem.Rnw:965-967
###################################################
table(psample$full.paired)
psample$full.paired[1:10]


###################################################
### code chunk number 43: cem.Rnw:971-977
###################################################
# cem match: automatic bin choice, we drop one row from the data set
mat1 <- cem(data=LL[-1,], drop="re78")

# we want a set of paired units but we have an odd number of units in the data
psample <- pair(mat1, data=LL[-1,])
table(psample$full.paired)


