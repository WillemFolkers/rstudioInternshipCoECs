# R/SQL Code Scan Distribution
scan_distribution <- sqldf("SELECT type_scan, count(type_scan) as amount
FROM scans
GROUP BY type_scan")
View(scan_distribution)

binom.test(2654, 2658,
alternative = c("two.sided", "less", "greater"),
conf.level = 0.95)

# Exact binomial test
# 
# data:  2654 and 2658
# number of successes = 2654, number of trials = 2658, p-value < 2.2e-16
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#   0.9961514 0.9995898
# sample estimates:
#   probability of success 
# 0.9984951

# R/SQL Code Scan Type Depending On
# Size:
type_size_distribution <- sqldf("SELECT company_size, type_scan, count(type_scan) as amount
                                FROM organizations_subscriptions
                                INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                GROUP BY company_size, type_scan")

type_size_distribution <- pivot_wider(type_size_distribution, names_from = type_scan, values_from = amount)

type_size_distribution[is.na(type_size_distribution)] <- 0

View(type_size_distribution)

type_size_answer <- chisq.test(type_size_distribution[1:3,c("auto","on-demand")], correct = TRUE)
# Warning message:
# In chisq.test(type_size_distribution[1:3, c("auto", "on-demand")],  :
# Chi-squared approximation may be incorrect

View(type_size_answer$expected)

type_size_answer

# Pearson's Chi-squared test
# 
# data:  type_size_distribution[1:3, c("auto", "on-demand")]
# X-squared = 0.37649, df = 2, p-value = 0.8284

# Sector
type_sector_distribution <- sqldf("SELECT name_en, type_scan, count(type_scan) as amount
                                  FROM organizations_subscriptions
                                  INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                  INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                  INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                  GROUP BY name_en, type_scan")

type_sector_distribution <- pivot_wider(type_sector_distribution, names_from = type_scan, values_from = amount)

type_sector_distribution[is.na(type_sector_distribution)] <- 0

View(type_sector_distribution)

type_sector_answer <- chisq.test(type_sector_distribution[1:21,c("auto","on-demand")], correct = TRUE)
# Warning message:
# In chisq.test(type_sector_distribution[1:21, c("auto", "on-demand")],  :
# Chi-squared approximation may be incorrect

View(type_sector_answer$expected)

type_sector_answer

# Pearson's Chi-squared test
# 
# data:  type_sector_distribution[1:21, c("auto", "on-demand")]
# X-squared = 29.157, df = 20, p-value = 0.08471

# Zip code
type_zipcode_distribution <- sqldf("SELECT zipcode, type_scan, count(type_scan) as amount
                                  FROM organizations_subscriptions
                                  INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                  INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                  WHERE zipcode != 'N/A'
                                  GROUP BY zipcode, type_scan")

type_zipcode_distribution <- pivot_wider(type_zipcode_distribution, names_from = type_scan, values_from = amount)

type_zipcode_distribution[is.na(type_zipcode_distribution)] <- 0

View(type_zipcode_distribution)

type_zipcode_answer <- chisq.test(type_zipcode_distribution[1:9,c("auto","on-demand")], correct = TRUE)
# Warning message:
# In chisq.test(type_zipcode_distribution[1:9, c("auto", "on-demand")],  :
#   Chi-squared approximation may be incorrect

View(type_zipcode_answer$expected)

par(mar=c(6,6,4,4))
barplot(height=t(data.frame(type_zipcode_distribution$'auto', type_zipcode_distribution$'on-demand')), ylim=c(0,200), names.arg=type_zipcode_distribution$zipcode, main='Scan type by company zipcode', legend=c('auto', 'on-demand'), col=c('blue', 'red'), xlab='Company scan type on zipcode', ylab='Amount of companies')

type_zipcode_answer

# Pearson's Chi-squared test
# 
# data:  type_zipcode_distribution[1:9, c("auto", "on-demand")]
# X-squared = 5.1967, df = 8, p-value = 0.7364

# Size and Sector
type_size_sector_distribution <- sqldf("SELECT company_size, name_en, type_scan, count(type_scan) as amount
                                        FROM organizations_subscriptions
                                        INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                        INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                        INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                        GROUP BY company_size, name_en, type_scan")

type_size_sector_distribution <- unite(type_size_sector_distribution, company_info, company_size:name_en, sep=' : ')

type_size_sector_distribution <- pivot_wider(type_size_sector_distribution, names_from = type_scan, values_from = amount)

type_size_sector_distribution[is.na(type_size_sector_distribution)] <- 0

View(type_size_sector_distribution)

type_size_sector_answer <- chisq.test(type_size_sector_distribution[1:46,c("auto","on-demand")], correct = TRUE)
# Warning message:
# In chisq.test(type_size_sector_distribution[1:46, c("auto", "on-demand")],  :
# Chi-squared approximation may be incorrect

View(type_size_sector_answer$expected)

type_size_sector_answer

# Pearson's Chi-squared test
# 
# data:  type_size_sector_distribution[1:46, c("auto", "on-demand")]
# X-squared = 30.114, df = 45, p-value = 0.9567
