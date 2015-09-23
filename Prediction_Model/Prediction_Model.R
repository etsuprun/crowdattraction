# read data
train <- read.csv(instagram)

# train a linear regression model
LM <- lm(total ~ hour + holiday + wday, data=train)

# predict result
pred <- predict(LM, newdata=test)

# insert the predicted result back to the test data set for plotting
test$pred <- pred

# format the data for ggplot
predPF <- test%>% group_by(hour) %>% summarize(crowd = sum(pred))
predPF <- data.frame(predPF)

# ggplot to plot the predicted crowd data
library(ggplot2)
g <- ggplot(predPF, aes=(x=hour, y=crowd))
g + geom_point()