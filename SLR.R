df <- read.csv('KDDTrain+.csv',header = FALSE)

#Removing unwanted Columns
##columns having long trails of zeros
drops <- c("V7","V8","V9","V11","V14","V15","V17","V18","V20","V21","V43")
df <- df[ , !(names(df) %in% drops)]



# Encoding into categorical variables as factors

df$V2 <- as.numeric(factor(df$V2,levels = c('icmp','udp','tcp'),labels = c(1,2,3)))

df$V3 <- as.numeric(factor(df$V3,levels = c("aol","auth","bgp","courier","csnet_ns","ctf" ,"daytime","discard","domain","domain_u","echo","eco_i","ecr_i","efs","exec","finger","ftp","ftp_data","gopher","harvest","hostnames","http","http_2784","http_443","http_8001","imap4","IRC","iso_tsap","klogin","kshell","ldap","link","login","mtp","name","netbios_dgm","netbios_ns","netbios_ssn","netstat","nnsp","nntp","ntp_u","other","pm_dump","pop_2","pop_3","printer","private","red_i","remote_job","rje","shell","smtp","sql_net","ssh"  ,"sunrpc","supdup","systat","telnet","tftp_u","tim_i" ,"time","urh_i","urp_i","uucp","uucp_path","vmnet","whois","X11","Z39_50"),labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,3,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70)))

df$V4 <- as.numeric(factor(df$V4,levels = c("OTH","REJ","RSTO","RSTOS0","RSTR","S0","S1","S2","S3","SF","SH"),labels = c(1,2,3,4,5,6,7,8,9,10,11)))

df$V42 <- as.numeric(factor(df$V42,levels = c("back","buffer_overflow","ftp_write","guess_passwd","imap","ipsweep","land","loadmodule","multihop","neptune","nmap","perl","phf","pod","portsweep","rootkit","satan","smurf","spy","teardrop","warezclient","warezmaster","normal"),labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)))

#Changing the attack type into boolean, 0 for normal and 1 for attack
#As it is a dependent Variable for the classification model
df$V42 <- ifelse(df$V42<23,1,0)


#splitting the dataset into Traing and Testing dataset
library(caTools)
set.seed(123)
split <- sample.split(df$V42,SplitRatio = 0.8)
train <- subset(df,split=TRUE)
test <- subset(df,split=FALSE)

#Feature Scaling
train[-32] <- scale(train[-32])#[-32]expect dependent variable
test[-32] <- scale(test[-32])



#Applying Principal Component Analysis for Feature extraction
library(caret)
library(e1071)
pca <- preProcess(x=train[-32],method = 'pca',pcaComp = 10)#selecting 10 PC's for further analysis

##fitting the PCA
train_pca <- predict(pca,train)
train_pca <- train_pca[c(2,3,4,5,6,7,8,9,10,1)]#making last coloumn dependent variable column

test_pca <- predict(pca,test)
test_pca <- test_pca[c(2,3,4,5,6,7,8,9,10,1)]#making last coloumn dependent variable column



##Fitting the ANN Model
##Using RProp+ learning Algorithm
library(neuralnet)
SLRann <- neuralnet(formula = V42~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9 ,data = train_pca , hidden = c(7,7),learningrate = NULL, lifesign = "full", threshold = 0.05, linear.output = FALSE, err.fct = "sse",lifesign.step = 1000,stepmax = 1e+05,algorithm = "slr",rep=1)


##Predicting the Test set results
y_pred = compute(SLRann, newdata = (test_pca[-10]))
y_pred = (y_pred > 0.5)
y_pred = as.vector(y_pred)


#Making the confusion matrix
cm <- table(test_pca[,10],predictor)
print(cm)
summary(cm)