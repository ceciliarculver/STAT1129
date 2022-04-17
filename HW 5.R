# HW 5 - STAT 1129

# Question 1
matrix1 = matrix(c(7,2,9,4,12,13), nrow = 2, ncol = 3)
matrix2 = matrix(c(1,2,3,7,8,9,12,13,14,19,20,21), nrow = 3, ncol = 4)
matrix3 = matrix1 %*% matrix2
matrix3

# Question 2
df = data.frame(
  id = c(1,2,3,4,5),
  name = c('Peter','Amy','Ryan','Gary','Michelle'),
  salary = c(623.30, 515.20, 611.00, 729.00, 843.25)
)
df$department = c('IT','Finance','Computer Science','HR','Managment')
df[c(1,3,5),c(2,3)]

barplot(df$salary, names.arg = df$name)

values = c(max(df$salary), min(df$salary), median(df$salary))
labels = c('Max','Min','Median')
colors = c('blue','pink','purple')
pie(values, labels = labels, main = 'Salary Pie Chart', col = colors)
legend('topleft', labels, fill = colors)

# Question 3
# data imported in separate step
head(Amazon)
amazon = Amazon[, c(1,16,17,18,21)]
amazon[is.na(amazon)] = 0

amazon$Total.Charged = as.numeric(sub("\\$", "",amazon$Total.Charged))
amazon$Subtotal = as.numeric(sub("\\$", "",amazon$Subtotal))
amazon$Shipping.Charge = as.numeric(sub("\\$", "",amazon$Shipping.Charge))
amazon$Tax.Before.Promotions = as.numeric(sub("\\$", "",
                                              amazon$Tax.Before.Promotions))
amazon$Order.Date <- as.Date(amazon$Order.Date , format = "%m/%d/%y")

dim(amazon)
head(amazon)

# SUMMARY STATS
print('Summary Stats of Total Charged')
paste('Max:', max(amazon$Total.Charged))
paste('Min:', min(amazon$Total.Charged))
paste('Median:', median(amazon$Total.Charged))
paste('Mean:', mean(amazon$Total.Charged))

library(ggplot2)
install.packages("lubridate")
library(lubridate)

#HISTOGRAM
hist(amazon$Total.Charged, xlab = 'Total Charged per Order', 
     main = 'Histogram of Price per Order', col = 'blue')

#MONTHLY SUMMARY / BAR PLOT
amazon$Month = month(amazon$Order.Date)
ggplot(amazon, aes(x = Month)) + geom_bar()
  + ggtitle('Monthly Orders')

#MULTI-LINE COMPARISON
ggplot(amazon, aes(x = Order.Date)) +
  geom_line(aes(y=Total.Charged, color="Total")) +
  geom_line(aes(y=Tax.Before.Promotions, color="Tax")) + 
  labs(color="Legend text") +
  ggtitle('Total vs Tax')

#PIE CHART
values = c(sum(amazon$Shipping.Charge), sum(amazon$Tax.Before.Promotions), 
                                            sum(amazon$Total.Charged))
labels = c('Shipping','Tax','Price')

colors = c('blue','pink','purple')
pie(values, labels = labels, main = 'Make-Up of Total Charged', col = colors)
legend('topleft', labels, fill = colors)






