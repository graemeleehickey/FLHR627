library(lme4)
library(ggplot2)
# all plots 700 x 500

# All data
ggplot(aes(x = Days, y = Reaction), data = sleepstudy) +
  geom_line(aes(colour = Subject)) +
  theme(legend.position = "none")

# + single fixed effects line
ggplot(aes(x = Days, y = Reaction), data = sleepstudy) +
  geom_line(aes(colour = Subject)) +
  geom_smooth(se = FALSE, method = "lm", colour = "black", size = 2) +
  theme(legend.position = "none")

# + individual intercepts
fit <- lm(Reaction ~ Subject + Days, data = sleepstudy)
xrange <- range(sleepstudy$Days)
xseq <- seq(from = xrange[1], to = xrange[2])
subj <- levels(sleepstudy$Subject)
newdata <- expand.grid(subj, xseq)
colnames(newdata) <- c("Subject", "Days")
newdata$Reaction <- predict(fit, newdata)
ggplot(aes(x = Days, y = Reaction), data = sleepstudy) +
  geom_line(aes(colour = Subject), alpha = 0.4) +
  geom_line(aes(x = Days, y = Reaction, colour = Subject), size = 1.5,
            data = newdata) +
  theme(legend.position = "none")

# + individual intercepts & slopes
ggplot(aes(x = Days, y = Reaction), data = sleepstudy) +
  geom_line(aes(colour = Subject), alpha = 0.4) +
  geom_smooth(aes(colour = Subject), se = FALSE, method = "lm", size = 1.5) +
  theme(legend.position = "none")

# shrinkage plot
fit0 <- lm(Reaction ~ Days, data = sleepstudy)
fit1 <- lm(Reaction ~ -1 + Subject + Subject:Days, data = sleepstudy)
fit2 <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)

N <- nlevels(sleepstudy$Subject)
ab1 <- data.frame("subject" = levels(sleepstudy$Subject),
                  "intercept" = coef(fit1)[1:N],
                  "slope" = coef(fit1)[(N+1):(2*N)],
                  "type" = "Within-subjects")
ab2 <- cbind("subject" = levels(sleepstudy$Subject),
             coef(fit2)$Subject,
             "type" = "Random-effects")
colnames(ab2)[2:3] <- c("intercept", "slope")
ab <- rbind(ab1, ab2)
rownames(ab) <- NULL

ab.wide <- reshape(ab, v.names = c("intercept", "slope"), 
                   idvar = "subject",
                   timevar = "type", 
                   direction = "wide")
names(ab.wide) <- c("subject", "intercept1", "slope1", "intercept2", "slope2")

ggplot(aes(x = intercept, y = slope), data = ab) +
  geom_point(aes(colour = type), size = 3) +
  geom_segment(aes(x = intercept1, xend = intercept2,
                   y = slope1, yend = slope2, type = 1),
               data = ab.wide,
               size = 1,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_vline(xintercept = coef(fit0)[1], colour = "red", size = 1.3,
             linetype = "dashed") +
  geom_hline(yintercept = coef(fit0)[2], colour = "red", size = 1.3,
             linetype = "dashed")

# write data for SPSS
library(haven)
write_sav(sleepstudy, "sleepstudy.sav")

