library(CausalImpact)
library(ggplot2)
library(bigrquery)
library(tidyverse)
library(bsts)
library(xts)
library(tbl2xts)



# store the project id
projectid = "source-data-314320"

# set your query
sql <- "SELECT 
          date,
          sum(CASE WHEN tienda = 'Centro' THEN sales ELSE 0 END) AS centro_sales,
          sum(CASE WHEN tienda = 'Pl.Patria' THEN sales ELSE 0 END) AS patria_sales
        FROM `source-data-314320.Store_Data.All_Data`
        WHERE Owner = 'M&D'
          AND date <= CURRENT_DATE()
          AND date >= '2021-09-01'
        GROUP BY date
        ORDER BY Date
"

# Run the query and store the data in a tibble
data_centro <- bq_project_query(projectid, sql)

bq_table_centro <- bq_table_download(data_centro)

# Print first rows of the data
bq_table_centro


# get the earliest (min) and latest (max) dates
earliest_date <- min(bq_table_centro$date)
latest_date <- max(bq_table_centro$date)


date.points <- data.frame(date = seq.Date(as.Date(earliest_date), by = 1, length.out = (latest_date - earliest_date) + 1))
merged <- merge(date.points, bq_table_centro, by = "date", all = TRUE)

xts <- merged %>%
  select(date, centro_sales, patria_sales) %>%
  tbl_xts(., cols_to_xts = c(centro_sales, patria_sales), Colnames_Exact = TRUE)


View(xts)

xts <- na.fill(xts, fill = 0.00)

table.xts <- xts_tbl(xts, Colnames_Exact = FALSE)
View(table.xts)


# visualize the data
matplot(table.xts,
        type = "l",
        col = 2,
        lwd = 2,
        lty = 1)

matlines(table.xts[, 2],
        type = "l",
        col = 1,
        lwd = 2,
        lty = 1)
        


# pre.period sets 30 days prior to latest date as training data. 30-day post-period used for predictions.
pre.period <- as.Date(c(earliest_date, latest_date - 30))
post.period <- as.Date(c(max(pre.period) + 1, latest_date))


impact <- CausalImpact(table.xts, pre.period, post.period)
plot(impact)





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





