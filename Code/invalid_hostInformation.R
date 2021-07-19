# R/SQL Code Invalid Host
invalid_host_distribution <- sqldf("SELECT invalid_host, count(invalid_host) as amount
                                    FROM scans
                                    GROUP BY invalid_host")
View(invalid_host_distribution)

binom.test(2626, 2658,
           alternative = c("two.sided", "less", "greater"),
           conf.level = 0.95)

# Exact binomial test
# 
# data:  2626 and 2658
# number of successes = 2626, number of trials = 2658, p-value < 2.2e-16
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#   0.9830465 0.9917511
# sample estimates:
#   probability of success 
# 0.9879609

# R/SQL Code Invalid Host Depending On
# Size:
  host_size_distribution <- sqldf("SELECT company_size, invalid_host, count(invalid_host) as amount
                                    FROM organizations_subscriptions
                                    INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                    INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                    GROUP BY company_size, invalid_host")

host_size_distribution <- pivot_wider(host_size_distribution, names_from = invalid_host, values_from = amount)

host_size_distribution[is.na(host_size_distribution)] <- 0

View(host_size_distribution)

host_size_answer <- chisq.test(host_size_distribution[1:3,c("0","1")], correct = FALSE)
# Warning message:
#   In chisq.test(host_size_distribution[1:3, c("0", "1")], correct = FALSE) :
#   Chi-squared approximation may be incorrect

View(host_size_answer$expected)

host_size_answer <- fisher.test(host_size_distribution[1:3,c("0","1")])

host_size_answer

# Fisher's Exact Test for Count Data
# 
# data:  host_size_distribution[1:3, c("0", "1")]
# p-value = 0.3885
# alternative hypothesis: two.sided

# Sector
host_sector_distribution <- sqldf("SELECT name_en, invalid_host, count(invalid_host) as amount
                                  FROM organizations_subscriptions
                                  INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                  INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                  INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                  GROUP BY name_en, invalid_host")

host_sector_distribution <- pivot_wider(host_sector_distribution, names_from = invalid_host, values_from = amount)

host_sector_distribution[is.na(host_sector_distribution)] <- 0

View(host_sector_distribution)

host_sector_answer <- chisq.test(host_sector_distribution[1:21,c("0","1")], correct = TRUE)
# Warning message:
# In chisq.test(host_sector_distribution[1:21, c("0", "1")], correct = TRUE) :
#   Chi-squared approximation may be incorrect

View(host_sector_answer$expected)

host_sector_answer

# 	Pearson's Chi-squared test
# 
# data:  host_sector_distribution[1:21, c("0", "1")]
# X-squared = 19.407, df = 20, p-value = 0.4955

# Zip code
host_zipcode_distribution <- sqldf("SELECT zipcode, invalid_host, count(invalid_host) as amount
                                    FROM organizations_subscriptions
                                    INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                    INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                    INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                    WHERE zipcode != 'N/A'
                                    GROUP BY zipcode, invalid_host")

host_zipcode_distribution <- pivot_wider(host_zipcode_distribution, names_from = invalid_host, values_from = amount)

host_zipcode_distribution[is.na(host_zipcode_distribution)] <- 0

View(host_zipcode_distribution)

host_zipcode_answer <- chisq.test(host_zipcode_distribution[1:9,c("0","1")], correct = TRUE)
# Warning message:
#   In chisq.test(host_zipcode_distribution[1:9, c("0", "1")], correct = TRUE) :
#   Chi-squared approximation may be incorrect

View(host_zipcode_answer$expected)

host_zipcode_answer

# Pearson's Chi-squared test
# 
# data:  host_zipcode_distribution[1:9, c("0", "1")]
# X-squared = 9.2488, df = 8, p-value = 0.3217

# Size and Sector
host_size_sector_distribution <- sqldf("SELECT company_size, name_en, invalid_host, count(invalid_host) as amount
                                        FROM organizations_subscriptions
                                        INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                        INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                        INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                        GROUP BY company_size, name_en, invalid_host")

host_size_sector_distribution <- unite(host_size_sector_distribution, company_info, company_size:name_en, sep=' : ')

host_size_sector_distribution <- pivot_wider(host_size_sector_distribution, names_from = invalid_host, values_from = amount)

host_size_sector_distribution[is.na(host_size_sector_distribution)] <- 0

View(host_size_sector_distribution)

host_size_sector_answer <- chisq.test(host_size_sector_distribution[1:46,c("0","1")], correct = TRUE)
# Warning message:
# In chisq.test(host_size_sector_distribution[1:46, c("0", "1")],  :
#   Chi-squared approximation may be incorrect

View(host_size_sector_answer$expected)

host_size_sector_answer

# 	Pearson's Chi-squared test
# 
# data:  host_size_sector_distribution[1:46, c("0", "1")]
# X-squared = 48.17, df = 45, p-value = 0.3459
