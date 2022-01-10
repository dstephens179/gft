library(CausalImpact)
library(ggplot2)
library(bsts)


##### creating an example dataset
set.seed(1)
x1 <- 100 + arima.sim(model = list(ar = 0.999), n = 100)
y <- 1.2 * x1 + rnorm(100)

# y increases by 10 starting at 71.
y[71:100] <- y[71:100] + 10

# cbind() combines Y and x1 columms in a dataframe.
data <- cbind(y, x1)

# check the dimensions of the data
dim(data)


# visualize the data
matplot(data, type = "l")



##### working with dates
time.points <- seq.Date(as.Date("2014-01-01"), by = 1, length.out = 100)
data <- zoo(cbind(y, x1), order.by = time.points)
head(data)


# specify the pre-/post-period dates for training & predictions, respectively.
pre.period <- as.Date(c("2014-01-01", "2014-03-11"))
post.period <- as.Date(c("2014-03-12", "2014-04-10"))


impact <- CausalImpact(data, pre.period, post.period)
plot(impact)


pred1 <- predict(impact, horizon = 100)


# print out the summary table
summary(impact)

# written, verbal summary
summary(impact, "report")

# precise numbers in the summary
impact$summary




impact.plot <- plot(impact) + theme_bw(base_size = 10)
plot(impact.plot)


























##### use the bsts package (also installed) instead of using the default from CausalImpact package.
# must set the observed data in post-treatment period to NA,
# showing the counterfactual response is unobserved after the intervention

post.period <- c(71,100)
post.period.response <- y[post.period[1] : post.period[2]]
y[post.period[1] : post.period[2]] <- NA


# set up and estimate a time-series model use bsts.
ss <- AddLocalLevel(list(),y)
bsts.model <- bsts(y ~ x1, ss, niter = 1000)


# now bring back CausalImpact, and pass in the fitted model object you just created.
# also provide the actual obversed response. This is so the package computes the difference between
# predicted response (bsts.model) and actual observed response (post.period.response)
impact <- CausalImpact(bsts.model = bsts.model,
                        post.period.response = post.period.response)


plot(impact)
summary(impact)
summary(impact, "report")





