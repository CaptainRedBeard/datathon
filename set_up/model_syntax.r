set.seed(4444)
partition <- sample(nrow(DATA_FRAME))
DATA_FRAME$group <- ifelse((partition < nrow(DATA_FRAME)/(3/2)), 1, 2)
DATA_FRAME$group <- factor(DATA_FRAME$group, levels = c(1,2), labels=c("train", "test"))
DATA_FRAME.train <- DATA_FRAME[(DATA_FRAME$group=="train"),]
DATA_FRAME.test <- DATA_FRAME[(DATA_FRAME$group=="test"),]


DATA_FRAME.train.md <- lm(dependent_variable ~ ind_variable + ..., data = DATA_FRAME.train)

DATA_FRAME.train.md1 <- gam(dependent_variable ~ ind_variable + s(ind_variable, k = {1-7}, bs = {"tp","ds","cs","cc","sos","cr") + ..., data=total.train)

DATA_FRAME.train.md2 <- loess(dependent_variable ~ ind_variable + ..., span = {0.25, 0.5, 0.75}, data = DATA_FRAME.train, control = loess.control(surface = 'direct'))

summary(DATA_FRAME.train.[md],[md1],[md2])

anova(DATA_FRAME.train.md, DATA_FRAME.train.md1, DATA_FRAME.train.md2)

DATA_FRAME.test$pred_value  <- predict(DATA_FRAME.train.md , newdata=DATA_FRAME.test)
DATA_FRAME.test$residuals <- DATA_FRAME$real_value - DATA_FRAME$pred_value
rmse <- sqrt(mean(DATA_FRAME.test$residuals^2))
