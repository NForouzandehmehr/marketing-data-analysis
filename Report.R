users<- read.csv(file="usersinfo.csv",head=TRUE,sep=",")
purchases <- read.csv(file="purchasesdata.csv",head=TRUE,sep=",")
messages <- read.csv(file="messagesdata.csv",head=TRUE,sep=",")

# Question 1
q1 <- merge(users, purchases, by='user.id')
q1$datediff <- as.Date(q1$purchase.date) - as.Date(q1$signup.date)
# >=0 because some purchases' dates are before the registration dates and we want to filter them out
q11 <- subset(q1, datediff <= 90 & datediff > 0)
#q11 <- subset(q1, (as.Date(purchase.date) - as.Date(signup.date)) <= 90 && (as.Date(purchase.date) - as.Date(signup.date)) > 0)
# I assume by percentage of users you mean the number of users that meet the specified criteria to the number of total registered users
# otherwise I could have obtained the denominator in the following line from purchases (# of unique user.ids) rather than from users
length(unique(q11[,1]))/length(users[,1])

# Question 2
q2 <- data.table(q11)
q22 <- q2[, min(as.Date(purchase.date)), by =c('user.id', 'signup.date')]
q23 <- merge(q22, messages, by='user.id')
q24 <- subset(q23, as.Date(signup.date) <= as.Date(message.date) & as.Date(V1) > as.Date(message.date))
length(unique(q24[1,]))/length(unique(q11[,1]))

# Question 3
q3 <- merge(users, messages, by='user.id')
#I assume first 90 days means first 90 days after registration
q31 <- subset(q3, as.Date(message.date) - as.Date(signup.date) <= 90 & as.Date(message.date) - as.Date(signup.date) > 0)
q32 <- data.table(q31)
#number of purchases within first 90 days
q33 <- q32[, sum(message.count), by='user.id']
#whether or not purchases something in the following 90 days
q34 <- subset(q1, as.Date(purchase.date) - as.Date(signup.date) <= 180 & as.Date(purchase.date) - as.Date(signup.date) > 90)
q35 <- data.table(q34)
q36 <- q35[, sum(purchase.count), by='user.id']
q37 <- merge(q33, q36, by='user.id', all.x = TRUE)
colnames(q37) <- c('user.id', 'mc', 'pc')
q38 <- data.frame(q37)
mean(subset(q38, is.na(pc))[,2])
mean(subset(q38, !is.na(pc))[,2])
q38[is.na(q38)] <- 0