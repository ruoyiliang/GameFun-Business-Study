#Read the dataset
GF <- read.csv("GameFun.csv")

#General assessment to the dataset
head(GF)
str(GF)

#Question 1 (the other questions are done using excel. Please find the excel file for the details).
summary(GF)
#Data preparation
#test = 1
test <- subset(GF, test == "1")
test_compare <- cbind(mean(test$income), mean(test$gender), mean(test$gamer))
colnames(test_compare) <- c("income_average", "gender_balancing", "gamer_balancing")
test_compare

#test = 0
control <- subset(GF, test == "0")
control_compare <- cbind(mean(control$income), mean(control$gender), mean(control$gamer))
control_compare

#merge the two into one df
compare <- rbind(test_compare, control_compare)
compare

#add the difference %
diff <- (control_compare/test_compare)-1
compare_result <- rbind(compare, diff)
rownames(compare_result) <- c("test", "control", "difference_ratio")
compare_result
t(compare_result)

#compute statistical significance
#income
income_test <- t.test(test$income, control$income, alternative = "two.sided", var.equal = TRUE)
income_test
#p-value > 0.05, should not reject null hypothesis. The two samples have equal variation.

#gender
#Since this variable is binomial, I use z-test here to compare the proportions of the two groups.
test_gender <- as.vector(test$gender)
control_gender <- as.vector(control$gender)

table(test_gender)
table(control_gender)

z_gender <- ((9908/28091)-(4210/11957))/
  ((((14118/40048)%*%(25930/40048)/28091)+
     ((14118/40048)%*%(25930/40048)/11957))^0.5)
z_gender
#|z| < 1.96, the difference between female and male is not significant at 5%.

#gamer
#Since this variable is binomial, I use z-test here to compare the proportions of the two groups.
table(test$gamer)
table(control$gamer)
nrow(test)
nrow(control)

z_gamer <- ((11199/28091)-(4761/11957))/
  ((((14118/40048)%*%(25930/40048)/28091)+
      ((15960/40048)%*%(24088/40048)/11957))^0.5)
z_gamer
#|z| < 1.96, the difference between non-gamer and gamer is not significant at 5%.













