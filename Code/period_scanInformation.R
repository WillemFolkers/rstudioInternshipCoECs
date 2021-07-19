# R/SQL Code Period Scan
period_scan_distribution <- sqldf("SELECT period_scan, count(period_scan) as amount
                                  FROM organizations_subscriptions
                                  GROUP BY period_scan")
View(period_scan_distribution)

probability <- c(1/3, 1/3, 1/3)
observed <- c(4, 2648, 6)
multinomial.test(observed, probability)

# Exact Multinomial Test, distance measure: p
# 
# Events    pObs    p.value
# 3536470       0          0

# R/SQL Code Period Scan Depending On
# Size:
period_size_distribution <- sqldf("SELECT company_size, period_scan, count(period_scan) as amount
                                  FROM lightscan JOIN organizations_subscriptions ON lightscan.id = id_lightscan
                                  GROUP BY company_size, period_scan")

period_size_distribution <- pivot_wider(period_size_distribution, names_from = period_scan, values_from = amount)

period_size_distribution[is.na(period_size_distribution)] <- 0

View(period_size_distribution)

period_size_answer <- chisq.test(period_size_distribution[1:3,c("none","halfyearly","quarterly")], correct = TRUE)
# Warning message:
# In chisq.test(period_size_distribution[1:3, c("none", "halfyearly",  :
# Chi-squared approximation may be incorrect

View(period_size_answer$expected)

period_size_answer

# Pearson's Chi-squared test
# 
# data:  period_size_distribution[1:3, c("none", "halfyearly", "quarterly")]
# X-squared = 26.182, df = 4, p-value = 2.908e-05

# Sector
period_sector_distribution <- sqldf("SELECT name_en, period_scan, count(period_scan) as amount
                                    FROM lightscan 
                                    INNER JOIN cyberstatus_branches ON cyberstatus_branches.id = lightscan.branche
                                    INNER JOIN organizations_subscriptions ON organizations_subscriptions.id_lightscan = lightscan.id
                                    GROUP BY name_en, period_scan")

period_sector_distribution <- pivot_wider(period_sector_distribution, names_from = period_scan, values_from = amount)

period_sector_distribution[is.na(period_sector_distribution)] <- 0

View(period_sector_distribution)

period_sector_answer <- chisq.test(period_sector_distribution[1:21,c("none","halfyearly","quarterly")], correct = TRUE)
# Warning message:
# In chisq.test(period_sector_distribution[1:21, c("none", "halfyearly",  :
# Chi-squared approximation may be incorrect

View(period_sector_answer$expected)

period_sector_answer

# Pearson's Chi-squared test
# 
# data:  period_sector_distribution[1:21, c("none", "halfyearly", "quarterly")]
# X-squared = 56.007, df = 40, p-value = 0.04774

# Zip code
period_zipcode_distribution <- sqldf("SELECT zipcode, period_scan, count(period_scan) as amount
                                    FROM lightscan 
                                    INNER JOIN cyberstatus_branches ON cyberstatus_branches.id = lightscan.branche
                                    INNER JOIN organizations_subscriptions ON organizations_subscriptions.id_lightscan = lightscan.id
                                    WHERE zipcode != 'N/A'
                                    GROUP BY zipcode, period_scan")

period_zipcode_distribution <- pivot_wider(period_zipcode_distribution, names_from = period_scan, values_from = amount)

period_zipcode_distribution[is.na(period_zipcode_distribution)] <- 0

View(period_zipcode_distribution)

period_zipcode_answer <- chisq.test(period_zipcode_distribution[1:9,c("none","quarterly")], correct = TRUE)
# Warning message:
# In chisq.test(period_zipcode_distribution[1:9, c("none", "quarterly")],  :
# Chi-squared approximation may be incorrect

View(period_zipcode_answer$expected)

period_zipcode_answer

# Pearson's Chi-squared test
# 
# data:  period_zipcode_distribution[1:9, c("none", "quarterly")]
# X-squared = 10.916, df = 8, p-value = 0.2065

# Size and Sector
period_size_sector_distribution <- sqldf("SELECT company_size, name_en, period_scan, count(period_scan) as amount
                                          FROM lightscan 
                                          INNER JOIN cyberstatus_branches ON cyberstatus_branches.id = lightscan.branche
                                          INNER JOIN organizations_subscriptions ON organizations_subscriptions.id_lightscan = lightscan.id
                                          GROUP BY company_size, name_en, period_scan")

period_size_sector_distribution <- unite(period_size_sector_distribution, company_info, company_size:name_en, sep=' : ')

period_size_sector_distribution <- pivot_wider(period_size_sector_distribution, names_from = period_scan, values_from = amount)

period_size_sector_distribution[is.na(period_size_sector_distribution)] <- 0

View(period_size_sector_distribution)

period_size_sector_answer <- chisq.test(period_size_sector_distribution[1:46,c("none","halfyearly","quarterly")], correct = TRUE)
# Warning message:
# In chisq.test(period_size_sector_distribution[1:46, c("none", "halfyearly",  :
# Chi-squared approximation may be incorrect

View(period_size_sector_answer$expected)

period_size_sector_answer

# Pearson's Chi-squared test
# 
# data:  period_size_sector_distribution[1:46, c("none", "halfyearly",     "quarterly")]
# X-squared = 167.4, df = 90, p-value = 1.375e-06
