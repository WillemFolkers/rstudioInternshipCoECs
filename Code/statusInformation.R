# R/SQL Code Status
status_distribution <- sqldf("SELECT status, count(status) as amount
                             FROM organizations_subscriptions
                             GROUP BY status")
View(status_distribution)

binom.test(2553, 2658,
           alternative = c("two.sided", "less", "greater"),
           conf.level = 0.95)

# Exact binomial test
# 
# data:  2553 and 2658
# number of successes = 2553, number of trials = 2658, p-value < 2.2e-16
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#   0.9523790 0.9675786
# sample estimates:
#   probability of success 
# 0.9604966

# R/SQL Code Status Depending On
# Size:
status_size_distribution <- sqldf("SELECT company_size, status, count(status) as amount
                                   FROM lightscan JOIN organizations_subscriptions ON lightscan.id = id_lightscan
                                   GROUP BY company_size, status")

status_size_distribution <- pivot_wider(status_size_distribution, names_from = status, values_from = amount)

status_size_distribution[is.na(status_size_distribution)] <- 0

View(status_size_distribution)

par(mar=c(6,6,4,4))
barplot(height=t(data.frame(status_size_distribution$'4', status_size_distribution$'5')), ylim=c(0,2500), names.arg=status_size_distribution$company_size, main='Status by company size', legend=c('4', '5'), col=c('blue', 'red'), xlab='Company status on size', ylab='Amount of companies')

status_size_answer <- chisq.test(status_size_distribution[1:3,c("4","5")], correct = TRUE)
# Warning message:
#   In chisq.test(status_size_distribution[1:3, c("4", "5")], correct = TRUE) :
#   Chi-squared approximation may be incorrect

View(status_size_answer$expected)

status_size_answer <- fisher.test(status_size_distribution[1:3,c("4","5")])

status_size_answer

# Fisher's Exact Test for Count Data
# 
# data:  status_size_distribution[1:3, c("4", "5")]
# p-value = 0.2036
# alternative hypothesis: two.sided

# Sector
status_sector_distribution <- sqldf("SELECT name_en, status, count(status) as amount
                                      FROM lightscan 
                                      INNER JOIN cyberstatus_branches ON cyberstatus_branches.id = lightscan.branche
                                      INNER JOIN organizations_subscriptions ON organizations_subscriptions.id_lightscan = lightscan.id
                                      GROUP BY name_en, status")

status_sector_distribution <- pivot_wider(status_sector_distribution, names_from = status, values_from = amount)

status_sector_distribution[is.na(status_sector_distribution)] <- 0

View(status_sector_distribution)

status_sector_answer <- chisq.test(status_sector_distribution[1:21,c("4","5")], correct = TRUE)
# Warning message:
# In chisq.test(status_sector_distribution[1:21, c("4", "5")], correct = TRUE) :
#   Chi-squared approximation may be incorrect

View(status_sector_answer$expected)

status_sector_answer

# 	Pearson's Chi-squared test
# 
# data:  status_sector_distribution[1:21, c("4", "5")]
# X-squared = 31.986, df = 20, p-value = 0.04344

# Zip code
status_zipcode_distribution <- sqldf("SELECT zipcode, status, count(status) as amount
                                      FROM lightscan 
                                      INNER JOIN cyberstatus_branches ON cyberstatus_branches.id = lightscan.branche
                                      INNER JOIN organizations_subscriptions ON organizations_subscriptions.id_lightscan = lightscan.id
                                      WHERE zipcode != 'N/A'
                                      GROUP BY zipcode, status")

status_zipcode_distribution <- pivot_wider(status_zipcode_distribution, names_from = status, values_from = amount)

status_zipcode_distribution[is.na(status_zipcode_distribution)] <- 0

View(status_zipcode_distribution)

status_zipcode_answer <- chisq.test(status_zipcode_distribution[1:9,c("4","5")], correct = TRUE)
# Warning message:
#   In chisq.test(status_zipcode_distribution[1:9, c("4", "5")], correct = TRUE) :
#   Chi-squared approximation may be incorrect

View(status_zipcode_answer$expected)

status_zipcode_answer

# Pearson's Chi-squared test
# 
# data:  status_zipcode_distribution[1:9, c("4", "5")]
# X-squared = 13.012, df = 8, p-value = 0.1114

# Size and Sector
status_size_sector_distribution <- sqldf("SELECT company_size, name_en, status, count(status) as amount
                                        FROM lightscan 
                                        INNER JOIN cyberstatus_branches ON cyberstatus_branches.id = lightscan.branche
                                        INNER JOIN organizations_subscriptions ON organizations_subscriptions.id_lightscan = lightscan.id
                                        GROUP BY company_size, name_en, status")

status_size_sector_distribution <- unite(status_size_sector_distribution, company_info, company_size:name_en, sep=' : ')

status_size_sector_distribution <- pivot_wider(status_size_sector_distribution, names_from = status, values_from = amount)

status_size_sector_distribution[is.na(status_size_sector_distribution)] <- 0

View(status_size_sector_distribution)

status_size_sector_answer <- chisq.test(status_size_sector_distribution[1:46,c("4","5")], correct = TRUE)
# Warning message:
# In chisq.test(status_size_sector_distribution[1:46, c("4", "5")],  :
#   Chi-squared approximation may be incorrect

View(status_size_sector_answer$expected)

status_size_sector_answer

# 	Pearson's Chi-squared test
# 
# data:  status_size_sector_distribution[1:46, c("4", "5")]
# X-squared = 61.879, df = 45, p-value = 0.04807
