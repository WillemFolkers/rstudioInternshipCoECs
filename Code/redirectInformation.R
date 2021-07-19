# R/SQL Code Redirect
redirect_distribution <- sqldf("SELECT redirect, count(redirect) as amount
                               FROM scans
                               GROUP BY redirect")
View(redirect_distribution)

binom.test(2439, 2658,
           alternative = c("two.sided", "less", "greater"),
           conf.level = 0.95)

# Exact binomial test
# 
# data:  2439 and 2658
# number of successes = 2439, number of trials = 2658, p-value < 2.2e-16
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#   0.9064999 0.9277841
# sample estimates:
#   probability of success 
# 0.9176072

# R/SQL Code Redirect Depending On
# Size:
  redirect_size_distribution <- sqldf("SELECT company_size, redirect, count(redirect) as amount
                                      FROM organizations_subscriptions
                                      INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                      INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                      GROUP BY company_size, redirect")

redirect_size_distribution <- pivot_wider(redirect_size_distribution, names_from = redirect, values_from = amount)

redirect_size_distribution[is.na(redirect_size_distribution)] <- 0

View(redirect_size_distribution)

par(mar=c(6,6,4,4))
barplot(height=t(data.frame(redirect_size_distribution$'0', redirect_size_distribution$'1')), ylim=c(0,2500), names.arg=redirect_size_distribution$company_size, main= 'Redirects by company size', legend=c('0', '1'), col=c('blue', 'red'), xlab='Company redirects on size', ylab='Amount of companies')

redirect_size_answer <- chisq.test(redirect_size_distribution[1:3,c("0","1")], correct = FALSE)
# Warning message:
#   In chisq.test(redirect_size_distribution[1:3, c("0", "1")], correct = FALSE) :
#   Chi-squared approximation may be incorrect

View(redirect_size_answer$expected)

redirect_size_answer <- fisher.test(redirect_size_distribution[1:3,c("0","1")])

redirect_size_answer

# Fisher's Exact Test for Count Data
# 
# data:  redirect_size_distribution[1:3, c("0", "1")]
# p-value = 0.2248
# alternative hypothesis: two.sided

# Sector
redirect_sector_distribution <- sqldf("SELECT name_en, redirect, count(redirect) as amount
                                      FROM organizations_subscriptions
                                      INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                      INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                      INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                      GROUP BY name_en, redirect")

redirect_sector_distribution <- pivot_wider(redirect_sector_distribution, names_from = redirect, values_from = amount)

redirect_sector_distribution[is.na(redirect_sector_distribution)] <- 0

View(redirect_sector_distribution)

redirect_sector_answer <- chisq.test(redirect_sector_distribution[1:21,c("0","1")], correct = TRUE)
# Warning message:
# In chisq.test(redirect_sector_distribution[1:21, c("0", "1")], correct = TRUE) :
#   Chi-squared approximation may be incorrect

View(redirect_sector_answer$expected)

redirect_sector_answer

# 	Pearson's Chi-squared test
# 
# data:  redirect_sector_distribution[1:21, c("0", "1")]
# X-squared = 21.161, df = 20, p-value = 0.3877

# Zip code
redirect_zipcode_distribution <- sqldf("SELECT zipcode, redirect, count(redirect) as amount
                                        FROM organizations_subscriptions
                                        INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                        INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                        INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                        WHERE zipcode != 'N/A'
                                        GROUP BY zipcode, redirect")

redirect_zipcode_distribution <- pivot_wider(redirect_zipcode_distribution, names_from = redirect, values_from = amount)

redirect_zipcode_distribution[is.na(redirect_zipcode_distribution)] <- 0

View(redirect_zipcode_distribution)

redirect_zipcode_answer <- chisq.test(redirect_zipcode_distribution[1:9,c("0","1")], correct = TRUE)
# Warning message:
#   In chisq.test(redirect_zipcode_distribution[1:9, c("0", "1")], correct = TRUE) :
# Chi-squared approximation may be incorrect

View(redirect_zipcode_answer$expected)

redirect_zipcode_answer

# Pearson's Chi-squared test
# 
# data:  redirect_zipcode_distribution[1:9, c("0", "1")]
# X-squared = 9.7977, df = 8, p-value = 0.2795

# Size and Sector
redirect_size_sector_distribution <- sqldf("SELECT company_size, name_en, redirect, count(redirect) as amount
                                            FROM organizations_subscriptions
                                            INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                            INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                            INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                            GROUP BY company_size, name_en, redirect")

redirect_size_sector_distribution <- unite(redirect_size_sector_distribution, company_info, company_size:name_en, sep=' : ')

redirect_size_sector_distribution <- pivot_wider(redirect_size_sector_distribution, names_from = redirect, values_from = amount)

redirect_size_sector_distribution[is.na(redirect_size_sector_distribution)] <- 0

View(redirect_size_sector_distribution)

redirect_size_sector_answer <- chisq.test(redirect_size_sector_distribution[1:46,c("0","1")], correct = TRUE)
# Warning message:
# In chisq.test(redirect_size_sector_distribution[1:46, c("0", "1")],  :
#   Chi-squared approximation may be incorrect

View(redirect_size_sector_answer$expected)

redirect_size_sector_answer

# 	Pearson's Chi-squared test
# 
# data:  redirect_size_sector_distribution[1:46, c("0", "1")]
# X-squared = 58.322, df = 45, p-value = 0.08781
