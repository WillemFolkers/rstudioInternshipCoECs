# R/SQL Code Firewall Blocked
firewall_blocked_distribution <- sqldf("SELECT firewall_blocked, count(firewall_blocked) as amount
                                       FROM organizations_subscriptions
                                       GROUP BY firewall_blocked")
View(firewall_blocked_distribution)

binom.test(1814, 2658,
           alternative = c("two.sided", "less", "greater"),
           conf.level = 0.95)

# Exact binomial test
# 
# data:  1814 and 2658
# number of successes = 1814, number of trials = 2658, p-value < 2.2e-16
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#   0.6643889 0.7001444
# sample estimates:
#   probability of success 
# 0.682468

# R/SQL Code Firewall Depending On
# Size:
firewall_size_distribution <- sqldf("SELECT company_size, firewall_blocked, count(firewall_blocked) as amount
                                   FROM lightscan JOIN organizations_subscriptions ON lightscan.id = id_lightscan
                                   GROUP BY company_size, firewall_blocked")

firewall_size_distribution <- pivot_wider(firewall_size_distribution, names_from = firewall_blocked, values_from = amount)

firewall_size_distribution[is.na(firewall_size_distribution)] <- 0

View(firewall_size_distribution)

par(mar=c(6,6,4,4))
barplot(height=t(data.frame(firewall_size_distribution$'0', firewall_size_distribution$'1')), ylim=c(0,2500), names.arg=firewall_size_distribution$company_size, main='Firewall blocks by company size', legend=c('0', '1'), col=c('blue', 'red'), xlab='Company scan firewall blocked on size', ylab='Amount of companies')

firewall_size_answer <- chisq.test(firewall_size_distribution[1:3,c("0","1")], correct = FALSE)

View(irewall_size_answer$expected)

firewall_size_answer

# Pearson's Chi-squared test
# 
# data:  firewall_size_distribution[1:3, c("0", "1")]
# X-squared = 18.912, df = 2, p-value = 7.82e-05

# Sector
firewall_sector_distribution <- sqldf("SELECT name_en, firewall_blocked, count(firewall_blocked) as amount
                                      FROM lightscan 
                                      INNER JOIN cyberstatus_branches ON cyberstatus_branches.id = lightscan.branche
                                      INNER JOIN organizations_subscriptions ON organizations_subscriptions.id_lightscan = lightscan.id
                                      GROUP BY name_en, firewall_blocked")

firewall_sector_distribution <- pivot_wider(firewall_sector_distribution, names_from = firewall_blocked, values_from = amount)

firewall_sector_distribution[is.na(firewall_sector_distribution)] <- 0

View(firewall_sector_distribution)

firewall_sector_answer <- chisq.test(firewall_sector_distribution[1:21,c("0","1")], correct = FALSE)
# Warning message:
# In chisq.test(firewall_sector_distribution[1:21, c("0", "1")], correct = FALSE) :
# Chi-squared approximation may be incorrect

View(firewall_sector_answer$expected)

firewall_sector_answer <- fisher.test(firewall_sector_distribution[1:21, c("0", "1")], simulate.p.value=TRUE)

firewall_sector_answer

# Fisher's Exact Test for Count Data with simulated p-value (based on 2000 replicates)
# 
# data:  firewall_sector_distribution[1:21, c("0", "1")]
# p-value = 0.0004998
# alternative hypothesis: two.sided

# Zip code
firewall_zipcode_distribution <- sqldf("SELECT zipcode, firewall_blocked, count(firewall_blocked) as amount
                                      FROM lightscan 
                                      INNER JOIN cyberstatus_branches ON cyberstatus_branches.id = lightscan.branche
                                      INNER JOIN organizations_subscriptions ON organizations_subscriptions.id_lightscan = lightscan.id
                                      WHERE zipcode != 'N/A'
                                      GROUP BY zipcode, firewall_blocked")

firewall_zipcode_distribution <- pivot_wider(firewall_zipcode_distribution, names_from = firewall_blocked, values_from = amount)

firewall_zipcode_distribution[is.na(firewall_zipcode_distribution)] <- 0

View(firewall_zipcode_distribution)

firewall_zipcode_answer <- chisq.test(firewall_zipcode_distribution[1:9,c("0","1")], correct = FALSE)

View(firewall_zipcode_answer$expected)

firewall_zipcode_answer

# Pearson's Chi-squared test
# 
# data:  firewall_zipcode_distribution[1:9, c("0", "1")]
# X-squared = 2.6217, df = 8, p-value = 0.9558

# Size and Sector
firewall_size_sector_distribution <- sqldf("SELECT company_size, name_en, firewall_blocked, count(firewall_blocked) as amount
                                          FROM lightscan 
                                          INNER JOIN cyberstatus_branches ON cyberstatus_branches.id = lightscan.branche
                                          INNER JOIN organizations_subscriptions ON organizations_subscriptions.id_lightscan = lightscan.id
                                          GROUP BY company_size, name_en,  firewall_blocked")

firewall_size_sector_distribution <- unite(firewall_size_sector_distribution, company_info, company_size:name_en, sep=' : ')

firewall_size_sector_distribution <- pivot_wider(firewall_size_sector_distribution, names_from = firewall_blocked, values_from = amount)

firewall_size_sector_distribution[is.na(firewall_size_sector_distribution)] <- 0

View(firewall_size_sector_distribution)

firewall_size_sector_answer <- chisq.test(firewall_size_sector_distribution[1:46,c("0","1")], correct = TRUE)
# Warning message:
# In chisq.test(firewall_size_sector_distribution[1:46, c("0", "1")],  :
# Chi-squared approximation may be incorrect

View(firewall_size_sector_answer$expected)

firewall_size_sector_answer

# Pearson's Chi-squared test
# 
# data:  firewall_size_sector_distribution[1:46, c("0", "1")]
# X-squared = 110.25, df = 45, p-value = 2.132e-07
