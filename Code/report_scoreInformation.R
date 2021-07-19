# R/SQL Code Report Score
report_score_distribution <- sqldf("SELECT report_score, count(report_score) as amount
FROM scans
GROUP BY report_score")
View(report_score_distribution)

# probability <- c(1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9)
# observed <- c(1121, 110, 38, 209, 787, 245, 1, 3, 144)
# multinomial.test(observed, probability)
# 
# Exact Multinomial Test, distance measure: p
# 
# Events    pObs    p.value
# 3536470       0          0

# R/SQL Code Report Score Depending On
# Size:
score_size_distribution <- sqldf("SELECT company_size, report_score, count(report_score) as amount
                                  FROM organizations_subscriptions
                                  INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                  INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                  GROUP BY company_size, report_score")

score_size_distribution <- score_size_distribution[order(score_size_distribution$report_score) , ]

score_size_distribution <- pivot_wider(score_size_distribution, names_from=report_score, values_from=amount)

score_size_distribution[is.na(score_size_distribution)] <- 0

View(score_size_distribution)

score_size_answer <- chisq.test(score_size_distribution[1:3,c("1","2", "3","4", "5","6", "7","8", "10")], correct = TRUE)
# Warning message:
# In chisq.test(score_size_distribution[1:3, c("1", "2", "3", "4",  :
# Chi-squared approximation may be incorrect

View(score_size_answer$expected)

score_size_answer

# Pearson's Chi-squared test
# 
# data:  score_size_distribution[1:3, c("1", "2", "3", "4", "5", "6",     "7", "8", "10")]
# X-squared = 22.505, df = 16, p-value = 0.1276

# Sector
score_sector_distribution <- sqldf("SELECT name_en, report_score, count(report_score) as amount
                                    FROM organizations_subscriptions
                                    INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                    INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                    INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                    GROUP BY name_en, report_score")

score_sector_distribution <- score_sector_distribution[order(score_sector_distribution$report_score) , ]

score_sector_distribution <- pivot_wider(score_sector_distribution, names_from= report_score, values_from=amount)

score_sector_distribution[is.na(score_sector_distribution)] <- 0

View(score_sector_distribution)

score_sector_answer <- chisq.test(score_sector_distribution[1:21,c("1","2", "3","4", "5","6", "7","8", "10")], correct = TRUE)
# Warning message:
# In chisq.test(score_sector_distribution[1:21, c("1", "2", "3", "4",  :
# Chi-squared approximation may be incorrect

View(score_sector_answer$expected)

score_sector_answer

# Pearson's Chi-squared test
# 
# data:  score_sector_distribution[1:21, c("1", "2", "3", "4", "5", "6",     "7", "8", "10")]
# X-squared = 200.28, df = 160, p-value = 0.01689

# Zip code
score_zipcode_distribution <- sqldf("SELECT zipcode, report_score, count(report_score) as amount
                                    FROM organizations_subscriptions
                                    INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                    INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                    INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                    WHERE zipcode != 'N/A'
                                    GROUP BY zipcode, report_score")

score_size_distribution <- score_size_distribution[order(score_size_distribution$report_score) , ]

score_zipcode_distribution <- pivot_wider(score_zipcode_distribution, names_from= report_score, values_from=amount)

score_zipcode_distribution[is.na(score_zipcode_distribution)] <- 0

View(score_zipcode_distribution)

score_zipcode_answer <- chisq.test(score_zipcode_distribution[1:9,c("1","2", "3","4", "5","6", "10")], correct = TRUE)
# Warning message:
# In chisq.test(score_zipcode_distribution[1:9, c("1", "2", "3", "4",  :
# Chi-squared approximation may be incorrect

View(score_zipcode_answer$expected)

score_zipcode_answer

# Pearson's Chi-squared test
# 
# data:  score_zipcode_distribution[1:9, c("1", "2", "3", "4", "5", "6",     "10")]
# X-squared = 41.989, df = 48, p-value = 0.7164

# Size and Sector
score_size_sector_distribution <- sqldf("SELECT company_size, name_en, report_score, count(report_score) as amount
                                        FROM organizations_subscriptions
                                        INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                        INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                        INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                        GROUP BY company_size, name_en, report_score")

score_size_sector_distribution <- unite(score_size_sector_distribution, company_info, company_size:name_en, sep=' : ')

score_size_sector_distribution <- score_size_sector_distribution[order(score_size_sector_distribution$report_score) , ]

score_size_sector_distribution <- pivot_wider(score_size_sector_distribution, names_from= report_score, values_from=amount)

score_size_sector_distribution[is.na(score_size_sector_distribution)] <- 0

View(score_size_sector_distribution)

score_size_sector_answer <- chisq.test(score_size_sector_distribution[1:46,c("1","2", "3","4", "5","6", "7","8", "10")], correct = TRUE)
# Warning message:
# In chisq.test(score_size_sector_distribution[1:46, c("1", "2", "3",  :
# Chi-squared approximation may be incorrect

View(score_size_sector_answer$expected)

score_size_sector_answer

# Pearson's Chi-squared test
# 
# data:  score_size_sector_distribution[1:46, c("1", "2", "3", "4", "5",     "6", "7", "8", "10")]
# X-squared = 306.8, df = 360, p-value = 0.9805
