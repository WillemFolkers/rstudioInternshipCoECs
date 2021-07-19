# R/SQL Code Skip Status
skip_status_distribution <-sqldf("SELECT skip_status, count(skip_status) as amount
                             FROM scans
                             GROUP BY skip_status")
View(skip_status_distribution)

binom.test(1810, 2658,
       alternative = c("two.sided", "less", "greater"),
       conf.level = 0.95)

# Exact binomial test
# 
# data:  1810 and 2658
# number of successes = 1810, number of trials = 2658, p-value < 2.2e-16
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#   0.6628634 0.6986635
# sample estimates:
#   probability of success 
# 0.6809631

# R/SQL Code Skip Status Depending On
# Size:
skip_size_distribution <- sqldf("SELECT company_size, skip_status, count(skip_status) as amount
                                FROM organizations_subscriptions
                                INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                GROUP BY company_size, skip_status")

skip_size_distribution <- pivot_wider(skip_size_distribution, names_from = skip_status, values_from = amount)

skip_size_distribution[is.na(skip_size_distribution)] <- 0

View(skip_size_distribution)

skip_size_answer <- chisq.test(skip_size_distribution[1:3,c("0","1")], correct = FALSE)

View(skip_size_answer$expected)

skip_size_answer

# Pearson's Chi-squared test
# 
# data:  skip_size_distribution[1:3, c("0", "1")]
# X-squared = 18.965, df = 2, p-value = 7.617e-05

# Sector
skip_sector_distribution <- sqldf("SELECT name_en, skip_status, count(skip_status) as amount
                                  FROM organizations_subscriptions
                                  INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                  INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                  INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                  GROUP BY name_en, skip_status")

skip_sector_distribution <- pivot_wider(skip_sector_distribution, names_from = skip_status, values_from = amount)

skip_sector_distribution[is.na(skip_sector_distribution)] <- 0

View(skip_sector_distribution)

par(mar=c(6,10,4,4))
barplot(height=t(data.frame(skip_sector_distribution$'0', skip_sector_distribution$'1')), xlim=c(0,500), names.arg=skip_sector_distribution$name_en, horiz=TRUE, cex.names=0.5, las=1, main='Invalid hosts by company sector', legend=c('0', '1'), col=c('blue', 'red'), xlab='Amount of companies')

skip_sector_answer <- chisq.test(skip_sector_distribution[1:21,c("0","1")], correct = FALSE)
# Warning message:
# In chisq.test(skip_sector_distribution[1:21, c("0", "1")], correct = FALSE) :
# Chi-squared approximation may be incorrect

View(skip_sector_answer$expected)

skip_sector_answer <- fisher.test(skip_sector_distribution[1:21,c("0","1")], simulate.p.value=TRUE)

skip_sector_answer

# Fisher's Exact Test for Count Data with simulated p-value (based on 2000 replicates)
# 
# data:  skip_sector_distribution[1:21, c("0", "1")]
# p-value = 0.0004998
# alternative hypothesis: two.sided

# Zip code
skip_zipcode_distribution <- sqldf("SELECT zipcode, skip_status, count(skip_status) as amount
                                    FROM organizations_subscriptions
                                    INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                    INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                    INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                    WHERE zipcode != 'N/A'
                                    GROUP BY zipcode, skip_status")

skip_zipcode_distribution <- pivot_wider(skip_zipcode_distribution, names_from = skip_status, values_from = amount)

skip_zipcode_distribution[is.na(skip_zipcode_distribution)] <- 0

View(skip_zipcode_distribution)

skip_zipcode_answer <- chisq.test(skip_zipcode_distribution[1:9,c("0","1")], correct = FALSE)

View(skip_zipcode_answer$expected)

skip_zipcode_answer

# Pearson's Chi-squared test
# 
# data:  skip_zipcode_distribution[1:9, c("0", "1")]
# X-squared = 2.7206, df = 8, p-value = 0.9506

# Size and Sector
skip_size_sector_distribution <- sqldf("SELECT company_size, name_en, skip_status, count(skip_status) as amount
                                        FROM organizations_subscriptions
                                        INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                        INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                        INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                        GROUP BY company_size, name_en, skip_status")

skip_size_sector_distribution <- unite(skip_size_sector_distribution, company_info, company_size:name_en, sep=' : ')

skip_size_sector_distribution <- pivot_wider(skip_size_sector_distribution, names_from = skip_status, values_from = amount)

skip_size_sector_distribution[is.na(skip_size_sector_distribution)] <- 0

View(skip_size_sector_distribution)

skip_size_sector_answer <- chisq.test(skip_size_sector_distribution[1:46,c("0","1")], correct = TRUE)
# Warning message:
# In chisq.test(skip_size_sector_distribution[1:46, c("0", "1")],  :
# Chi-squared approximation may be incorrect

View(skip_size_sector_answer$expected)

skip_size_sector_answer

# Pearson's Chi-squared test
# 
# data:  skip_size_sector_distribution[1:46, c("0", "1")]
# X-squared = 110.56, df = 45, p-value = 1.935e-07
