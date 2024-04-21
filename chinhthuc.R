library(magrittr)
library(dplyr)
library(ggplot2)
library(BMA)
library(relaimpo)
library(corrplot)


# Doc du lieu tu file .csv va quyet dinh chon bien
All_GPUs <- read.csv("C:/Users/Hai/OneDrive/Desktop/r/All_GPUs.csv")
?All_GPUs
# xem 6 dong dau cua file du lieu
head(All_GPUs)
# xem cau truc cua doi tuong du lieu new_GPUs
str(All_GPUs)
# hien thi ten cac cot trong khung du lieu 
names(All_GPUs)


#######################################################################3:tien xu li du lieu##############################################################################
# start
# lam sach du lieu, giu lai cac bien chinh can thiet
new_GPUs <- All_GPUs[,c("Core_Speed","Max_Power","Memory","Memory_Bandwidth","Memory_Speed","Process")]
# xem 5 dong dau tien cua file du lieu sau loc 
head(new_GPUs)
str(new_GPUs)

#lam sach cac bien du lieu
#ham kiem tra don vi
check_units <- function(a, b) {
  check <- new_GPUs[, c(a)]
  print(check)
  
  #regex
  valid_indices <- grep(paste0("[^ ]+ ", b), check)
  
  if (length(valid_indices) > 0) {
    
    print("Tat ca cac gia tri deu hop le")
  } else {
    print("Cac gia tri khong hop le la:")
    invalid_indices <- setdiff(seq_along(check), valid_indices)
    invalid_values <- check[invalid_indices]
    print(invalid_values)
    
  }
}
#########################################################3.1: Core_Speed##########################################################
#don vi
check_units("Core_Speed", "MHz")
new_GPUs$Core_Speed <- as.numeric(gsub(" MHz", "", new_GPUs$Core_Speed))
print(new_GPUs$Core_Speed)
summary(new_GPUs$Core_Speed)
#thay the NA
View(new_GPUs$Core_Speed)
new_GPUs$Core_Speed <- ifelse(is.na(new_GPUs$Core_Speed), median(new_GPUs$Core_Speed, na.rm=TRUE), new_GPUs$Core_Speed)
print(new_GPUs$Core_Speed)

ggplot(new_GPUs,aes(x="",y=new_GPUs$Core_Speed))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="springgreen",outlier.color="red")

##########################################################3.2: Memory##############################################################
check_units("Memory", "MB")
new_GPUs$Memory <- as.numeric(gsub("MB","",new_GPUs$Memory))
print(new_GPUs$Memory)
summary(new_GPUs$Memory)
#thay the NA
View(new_GPUs$Memory)
new_GPUs$Memory <- ifelse(is.na(new_GPUs$Memory), median(new_GPUs$Memory, na.rm=TRUE), new_GPUs$Memory)
print(new_GPUs$Memory)
ggplot(new_GPUs,aes(x="",y=new_GPUs$Memory))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="springgreen",outlier.color="red")

# Xac dinh khoang tu phan vi tren va duoi
checkMemory <- new_GPUs$Memory
lower_quantile <- quantile(checkMemory, 0.25)
upper_quantile <- quantile(checkMemory, 0.75)

# Xac dinh khoang tu phan vi
iqr <- upper_quantile - lower_quantile

# Xac dinh khoang ngoai lai
lower_threshold <- lower_quantile - 1.5 * iqr
upper_threshold <- upper_quantile + 1.5 * iqr

# Loai bo khoang ngoai lai
new_GPUs <- subset(new_GPUs, checkMemory >= lower_threshold & checkMemory <= upper_threshold)

# Sau khi xu li
summary(new_GPUs$Memory)
ggplot(new_GPUs,aes(x="",y=new_GPUs$Memory))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="springgreen",outlier.color="red")
print(new_GPUs$Memory)#
################################################################3.3: Memory_Speed##############################################################################################
#don vi
check_units("Memory_Speed", "MHz")
new_GPUs$Memory_Speed <- as.numeric(gsub("MHz","",new_GPUs$Memory_Speed))
print(new_GPUs$Memory_Speed)
summary(new_GPUs$Memory_Speed)
#thay the NA
View(new_GPUs$Memory_Speed)
new_GPUs$Memory_Speed <- ifelse(is.na(new_GPUs$Memory_Speed), median(new_GPUs$Memory_Speed, na.rm=TRUE), new_GPUs$Memory_Speed)
print(new_GPUs$Memory_Speed)

ggplot(new_GPUs,aes(x="",y=new_GPUs$Memory_Speed))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="springgreen",outlier.color="red")

############################################################3.4: Process##################################################################################################
check_units_process <- function(a, b) {
  check <- new_GPUs[, a]
  print(check)
  
  # Regex
  valid_indices <- grep("^\\d+nm$", check)
  valid_values <- check[valid_indices]
  
  if (length(valid_indices) > 0) {
    print("Tat ca gia tri deu hop le")
  } else {
    print("Cac gia tri khong hop le la:")
    invalid_indices <- setdiff(seq_along(check), valid_indices)
    invalid_values <- check[invalid_indices]
    print(invalid_values)
  }
}
#don vi
check_units_process("Process", "nm")
new_GPUs$Process <- as.numeric(gsub("nm","",new_GPUs$Process))
print(new_GPUs$Process)
summary(new_GPUs$Process)
#thay the NA
View(new_GPUs$Process)
new_GPUs$Process <- ifelse(is.na(new_GPUs$Process), median(new_GPUs$Process, na.rm=TRUE), new_GPUs$Process)
print(new_GPUs$Process)

ggplot(new_GPUs,aes(x="",y=new_GPUs$Process))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="springgreen",outlier.color="red")

# Xac dinh khoang tu phan vi tren va duoi
checkProcess <- new_GPUs$Process
lower_quantile <- quantile(checkProcess, 0.25)
upper_quantile <- quantile(checkProcess, 0.75)

# Xac dinh khoang tu phan vi
iqr <- upper_quantile - lower_quantile

# Xac dinh khoang ngoai lai
lower_threshold <- lower_quantile - 1.5 * iqr
upper_threshold <- upper_quantile + 1.5 * iqr

# Loai bo khoang ngoai lai
new_GPUs <- subset(new_GPUs, checkProcess >= lower_threshold & checkProcess <= upper_threshold)

# Sau khi xu li
summary(new_GPUs$Process)
ggplot(new_GPUs,aes(x="",y=new_GPUs$Process))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="springgreen",outlier.color="red")
print(new_GPUs$Process)
#############################################3.5: Memory Bandwidh#########################################################

check_units_bandwidth <- function(a, b) {
  check <- new_GPUs[, a]
  print(check)
  
  # Regex
  valid_indices <- grep("^\\d+(MB/sec|GB/sec)$", check)
  valid_values <- check[valid_indices]
  
  if (length(valid_indices) > 0) {
    print("Tất cả các giá trị đều hợp lệ.")
  } else {
    invalid_indices <- setdiff(seq_along(check), valid_indices)
    invalid_values <- check[invalid_indices]
    print(invalid_values)
  }
}
#don vi
check_units_bandwidth("Memory_Bandwidth", "const")

# Memory_Bandwidth bi sai don vi, chuyen doi tu MB/sec thanh GB/sec
temp <- grep("MB/sec",new_GPUs$Memory_Bandwidth)
for(i in temp) new_GPUs$Memory_Bandwidth[i] <- as.numeric(gsub("MB/sec","",new_GPUs$Memory_Bandwidth[i]))/1024
temp <- grep("GB/sec",new_GPUs$Memory_Bandwidth)
for (i in temp) new_GPUs$Memory_Bandwidth[i] <- as.numeric(gsub("GB/sec","",new_GPUs$Memory_Bandwidth[i]))
new_GPUs$Memory_Bandwidth <- as.numeric(gsub("GB/sec","",new_GPUs$Memory_Bandwidth))

print(new_GPUs$Memory_Bandwidth)
summary(new_GPUs$Memory_Bandwidth)
#thay the NA
View(new_GPUs$Memory_Bandwidth)
new_GPUs$Memory_Bandwidth <- ifelse(is.na(new_GPUs$Memory_Bandwidth), median(new_GPUs$Memory_Bandwidth, na.rm=TRUE), new_GPUs$Memory_Bandwidth)
print(new_GPUs$Process)

ggplot(new_GPUs,aes(x="",y=new_GPUs$Memory_Bandwidth))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="springgreen",outlier.color="red")

# Xac dinh khoang tu phan vi tren va duoi
checkBw <- new_GPUs$Memory_Bandwidth
lower_quantile <- quantile(checkBw, 0.25)
upper_quantile <- quantile(checkBw, 0.75)

# Xac dinh khoang tu phan vi
iqr <- upper_quantile - lower_quantile

# Xac dinh khoang ngoai lai
upper_threshold <- upper_quantile + 1.5 * iqr

# Loai bo khoang ngoai lai
new_GPUs <- subset(new_GPUs,  checkBw <= upper_threshold)

# Sau khi xu li
summary(new_GPUs$Memory_Bandwidth)
ggplot(new_GPUs,aes(x="",y=new_GPUs$Memory_Bandwidth))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="springgreen",outlier.color="red")
print(new_GPUs$Memory_Bandwidth)

##################################################3.6: Max_Power ######################################################################################
#don vi
check_units("Max_Power", "Watts")
new_GPUs$Max_Power <- as.numeric(gsub("Watts","",new_GPUs$Max_Power))
print(new_GPUs$Max_Power)
summary(new_GPUs$Max_Power)
#thay the NA
View(new_GPUs$Max_Power)
new_GPUs$Max_Power <- ifelse(is.na(new_GPUs$Max_Power), median(new_GPUs$Max_Power, na.rm=TRUE), new_GPUs$Max_Power)
print(new_GPUs$Max_Power)

########################check outliers
ggplot(new_GPUs,aes(x="",y=new_GPUs$Max_Power))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="springgreen",outlier.color="red")
########################loai outliers
# Xac dinh khoang tu phan vi tren va duoi
checkMP <- new_GPUs$Max_Power
lower_quantile <- quantile(checkMP, 0.25)
upper_quantile <- quantile(checkMP, 0.75)
# Xac dinh khoang tu phan vi
iqr <- upper_quantile - lower_quantile
# Xac dinh khoang ngoai lai
lower_threshold <- lower_quantile - 1.5 * iqr
upper_threshold <- upper_quantile + 1.5 * iqr
# Loai bo khoang ngoai lai
new_GPUs <- subset(new_GPUs, checkMP >= lower_threshold & checkMP <= upper_threshold)
# Sau khi xu li
summary(new_GPUs$Memory)
ggplot(new_GPUs,aes(x="",y=new_GPUs$Max_Power))+stat_boxplot(geom="errorbar",width=0.2)+geom_boxplot(fill="springgreen",outlier.color="red")
print(new_GPUs$Max_Power)

################################################3.7: check lan cuoi######################################################################
# Hien thi du lieu sau khi thuc hien
head(new_GPUs)
# thong ke vi tri dong chua du lieu khuyet
apply(is.na(new_GPUs),2,which)
# thong ke so luong du lieu khuyet trong cac bien
apply(is.na(new_GPUs),2,sum)
# kiem tra lai so luong du lieu khuyet thieu 
apply(is.na(new_GPUs),2,which)
# kiem tra cac cot co so nao am khong 
any(new_GPUs$Core_Speed < 0)
any(new_GPUs$Memory < 0)
any(new_GPUs$Memory_Bandwidth < 0)
any(new_GPUs$Memory_Speed < 0)
any(new_GPUs$Process < 0)
any(new_GPUs$Max_Power < 0)
# kiem tra du lieu trung lap dung lenh distinct() theo hinh thuc pipeline
str(new_GPUs)
new_GPUs <- new_GPUs %>% dplyr::distinct()
str(new_GPUs)
# chuyen doi cac bien sang dang log
new_GPUs2 <- new_GPUs
new_GPUs2[,c("Memory","Core_Speed","Max_Power","Memory_Bandwidth","Memory_Speed","Process")] <- log(new_GPUs2[,c("Memory","Core_Speed","Max_Power","Memory_Bandwidth","Memory_Speed","Process")])
head(new_GPUs2)
#####################################################################################end tien xu li du lieu####################################################################################

#########################################################################4: bieu thi du lieu################################################################
# Sử dụng lapply() để áp dụng hàm summary() cho mỗi biến
thong_ke <- lapply(new_GPUs, summary)

# In kết quả
print(thong_ke)
hist(new_GPUs$Memory_Speed,xlab = "Memory Speed",ylab = "Frequency",col = "lightblue",main = "Histogram of Memory Speed",xlim = c(0,2500))
hist(new_GPUs2$Memory_Speed,xlab = "Memory Speed",ylab = "Frequency",col = "lightblue",main = "Histogram of log(Memory Speed)",xlim = c(4,8))

hist(new_GPUs$Core_Speed,xlab = "Core Speed",ylab = "Frequency",col = "lightblue",main = "Histogram of Core Speed",xlim = c(0,2000))
hist(new_GPUs2$Core_Speed,xlab = "Core Speed",ylab = "Frequency",col = "lightblue",main = "Histogram of log(Core Speed)",xlim = c(4,8))

hist(new_GPUs$Max_Power,xlab = "Max Power",ylab = "Frequency",col = "lightblue",main = "Histogram of Max Power",xlim = c(0,300))
hist(new_GPUs2$Max_Power,xlab = "Max Power",ylab = "Frequency",col = "lightblue",main = "Histogram of log(Max Power)",xlim = c(0,7))

hist(new_GPUs$Memory,xlab = "Memory",ylab = "Frequency",col = "lightblue",main = "Histogram of Memory",xlim = c(0,9000))
hist(new_GPUs2$Memory,xlab = "Memory",ylab = "Frequency",col = "lightblue",main = "Histogram of log(Memory)",xlim = c(2,10))

hist(new_GPUs$Memory_Bandwidth,xlab = "Memory Bandwidth",ylab = "Frequency",col = "lightblue",main = "Histogram of Memory Bandwidth",xlim = c(0,400))
hist(new_GPUs2$Memory_Bandwidth,xlab = "Memory Bandwidth",ylab = "Frequency",col = "lightblue",main = "Histogram of log(Memory Bandwidth)",xlim = c(0,6))

hist(new_GPUs$Process,xlab = "Process",ylab = "Frequency",col = "lightblue",main = "Histogram of Process",xlim = c(10,60))
hist(new_GPUs2$Process,xlab = "Process",ylab = "Frequency",col = "lightblue",main = "Histogram of log(Process)",xlim = c(2,5))

#Vẽ biểu đồ correlation
correlation_matrix <- cor(new_GPUs)

# Vẽ biểu đồ tương quan
corrplot(correlation_matrix, method = "circle", 
         type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, 
         diag = TRUE, 
         addCoef.col = "black")
######################################################################end bieu dien du lieu####################################################################



###############################################################multiple linear regression############################################

###############################################################5: choose module###############################################
cols=colnames(new_GPUs2)
yvar=new_GPUs2[,("Memory_Speed")]
str(yvar)
xvars <- new_GPUs2[, !(colnames(new_GPUs2) %in% "Memory_Speed"), drop = FALSE]
bma=bicreg(xvars,yvar,strict=F,OR=2)
print(summary(bma))
##################################find weight######################
m=lm(Memory_Speed~Memory+Core_Speed+Max_Power+Memory_Bandwidth+Process, data=new_GPUs2)
calc.relimp(m,type="lmg",rela=T, rank=T)
#################################make training data and test data###############################
set.seed(42)
trainingRowIndex<-sample(1:nrow(new_GPUs2),0.8*nrow(new_GPUs2))
trainingData<-new_GPUs2[trainingRowIndex,]
testData<-new_GPUs2[-trainingRowIndex,]
print(nrow(trainingData))
print(nrow(testData))
################################xay dung mo hinh###################################
#tao mo hinh tu training data
lmMod<-lm(Memory_Speed~Memory+Core_Speed+Max_Power+Memory_Bandwidth+Process, data=new_GPUs2)
cPred<-predict(lmMod,testData)
#mean square error tu mo hinh
mse<-mean(lmMod$residuals^2)
#mean square error tu testData
mse_test=mean((testData$Memory_Speed-cPred)^2)
#iN MSE
print(paste("mse cua mo hinh: ",mse))
print(paste("mse cua TestData: ",mse_test))

##############################tim m1,m2,.. b#################################
print(summary(lmMod))

b<-coef(lmMod)[1]
mMemory<-coef(lmMod)[2]
mCore_Speed<-coef(lmMod)[3]
mMax_Power<-coef(lmMod)[4]
mMemory_Bandwidth<-coef(lmMod)[5]
mProcess<-coef(lmMod)[6]
############################ve bieu do###################################
# do thi bieu thi sai so hoi quy residuals va gia tri du bao fitted values
par(mfrow =c(2,2))
plot(lmMod)
par(mfrow = c(1,1))
##############################so sanh voi du lieu mau######################
Memory_Speed=testData$Memory_Speed
Memory=testData$Memory
Core_Speed=testData$Core_Speed
Max_Power=testData$Max_Power
Memory_Bandwidth=testData$Memory_Bandwidth
Process=testData$Process

data_predict<-data.frame(Memory,Core_Speed,Max_Power,Memory_Bandwidth,Process)
P<-lm(Memory_Speed~.,data=data_predict)
summary(P)
predict_Memory_Speed=predict(P)
p=data.frame(predict_Memory_Speed,Memory_Speed)
p

# du bao 
new <- new_GPUs2[,c("Memory","Core_Speed","Max_Power","Memory_Bandwidth","Memory_Speed","Process")]
result <- predict(lmMod, newdata = new, interval = "confidence")
newDf <- data.frame(ifelse(abs(result[,"fit"] - new_GPUs2[,"Memory_Speed"]) >= 0.5,FALSE, TRUE))
colnames(newDf) <- "Good predict"
apply(newDf,2,mean)
#############################Buoc 6: du doan mau##########################################################
#du doan Memory_Speed khi Memory=24576MB, Core_Speed=2235MHz, Max_Power=450 Watts, Memory_Bandwidth=1000GB/sec, Process=8nm
#tao dataframe
x2<-24000;x3<-2235;x4<-450;x5<-1000;x6<-8
y1<-data.frame("Memory"=log(x2),"Core_Speed"=log(x3),"Max_Power"=log(x4),"Memory_Bandwidth"=log(x5),"Process"=log(x6))
predict_X<-predict(lmMod,y1,interval="confidence")
head(predict_X)
print("Solution: ")
print(exp(predict_X))