install.packages('arules')
library(arules)

DDB <- read.table("DemographicsDB.txt", sep = "\t", header = TRUE)

DDB <- na.omit(DDB)

install.packages('fastDummies')
library(fastDummies)

DDB <-dummy_cols(DDB, 
                 select_columns = names(DDB), 
                 remove_selected_columns = TRUE)

DDB <- as.matrix(DDB)

DDB <- as(DDB,"transactions")

rules <- apriori(DDB,
                 parameter = list(supp = 0.1,
                                conf = 0.8,
                                minlen = 2,
                                maxlen = 5))

summary(rules)

#20 highest support rules
inspect(sort(rules,
             decreasing = T,
             by = "support")[1:20])

inspect(sort(rules,
             decreasing = T,
             by = "confidence")[1:20])

inspect(sort(rules,
             decreasing = T,
             by = "lift")[1:20])


div.rules <- apriori(DDB,
                     parameter=list(supp = .01,
                                    conf = .4,
                                    minlen = 2), 
                     appearance = list(default = "lhs", 
                                       rhs = "marstat_3"))

inspect(head(sort(div.rules,
                  decreasing = T,
                  by = "confidence")
             )
        )


ed1.rules <- apriori(DDB,
                     parameter = list(supp = 0.01,
                                      conf = 0.8, minlen = 2), 
                     appearance = list(lhs = "educ_1", default = "rhs"))

inspect(head(ed1.rules))

itemFrequencyPlot(DDB,
                  topN = 20,
                  type = "relative",
                  main = "Rel. Item Freq. Plot")

install.packages("arulesViz")
library(arulesViz)

install.packages("plotly")
library(plotly)
plot(rules)


subrules <- rules[quality(rules)$confidence>0.95]
plot(subrules)

plot(subrules,
     method = "two-key plot")


top10subrules <- head(subrules,
                      n = 10,
                      by = "confidence")


plot(top10subrules,
     method = "graph",
     engine = "htmlwidget")

subrules2 <- head(subrules,
                  n = 20,
                  by = "lift")

plot(subrules2,
     method = "paracoord")

