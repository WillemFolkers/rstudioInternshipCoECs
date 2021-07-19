# R/SQL Code No Open Ports
no_open_porst_distribution <- sqldf("SELECT no_open_ports, count(no_open_ports) as amount
                     FROM scans
                     GROUP BY no_open_ports")
View(no_open_porst_distribution)

binom.test(2636, 2658,
alternative = c("two.sided", "less", "greater"),
conf.level = 0.95)

# Exact binomial test
# 
# data:  2636 and 2658
# number of successes = 2636, number of trials = 2658, p-value < 2.2e-16
# alternative hypothesis: true probability of success is not equal to 0.5
# 95 percent confidence interval:
#   0.9874953 0.9948059
# sample estimates:
#   probability of success 
# 0.9917231

# R/SQL Code No Open Ports Depending On
# Size:
ports_size_distribution <- sqldf("SELECT company_size, no_open_ports, count(no_open_ports) as amount
                                  FROM organizations_subscriptions
                                  INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                  INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                  GROUP BY company_size, no_open_ports")

ports_size_distribution <- pivot_wider(ports_size_distribution, names_from = no_open_ports, values_from = amount)

ports_size_distribution[is.na(ports_size_distribution)] <- 0

View(ports_size_distribution)

ports_size_answer <- chisq.test(ports_size_distribution[1:3,c("0","1")], correct = FALSE)
# Warning message:
#   In chisq.test(ports_size_distribution[1:3, c("0", "1")], correct = TRUE) :
#   Chi-squared approximation may be incorrect

View(ports_size_answer$expected)

ports_size_answer <- fisher.test(ports_size_distribution[1:3,c("0","1")])

ports_size_answer

# Fisher's Exact Test for Count Data
# 
# data:  ports_size_distribution[1:3, c("0", "1")]
# p-value = 0.2569
# alternative hypothesis: two.sided

# Sector
ports_sector_distribution <- sqldf("SELECT name_en, no_open_ports, count(no_open_ports) as amount
                                  FROM organizations_subscriptions
                                  INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                  INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                  INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                  GROUP BY name_en, no_open_ports")

ports_sector_distribution <- pivot_wider(ports_sector_distribution, names_from = no_open_ports, values_from = amount)

ports_sector_distribution[is.na(ports_sector_distribution)] <- 0

View(ports_sector_distribution)

ports_sector_answer <- chisq.test(ports_sector_distribution[1:21,c("0","1")], correct = TRUE)
# Warning message:
# In chisq.test(ports_sector_distribution[1:21, c("0", "1")], correct = TRUE) :
#   Chi-squared approximation may be incorrect

View(ports_sector_answer$expected)

ports_sector_answer

# 	Pearson's Chi-squared test
# 
# data:  ports_sector_distribution[1:21, c("0", "1")]
# X-squared = 25.663, df = 20, p-value = 0.1772

# Zip code
ports_zipcode_distribution <- sqldf("SELECT zipcode, no_open_ports, count(no_open_ports) as amount
                                    FROM organizations_subscriptions
                                    INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                    INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                    INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                    WHERE zipcode != 'N/A'
                                    GROUP BY zipcode, no_open_ports")

ports_zipcode_distribution <- pivot_wider(ports_zipcode_distribution, names_from = no_open_ports, values_from = amount)

ports_zipcode_distribution[is.na(ports_zipcode_distribution)] <- 0

View(ports_zipcode_distribution)

par(mar=c(6,6,4,4))
barplot(height=t(data.frame(ports_zipcode_distribution$'0', ports_zipcode_distribution$'1')), ylim=c(0,200), names.arg=ports_zipcode_distribution$zipcode, main='No open ports by company zipcode', legend=c('0', '1'), col=c('blue', 'red'), xlab='Company no open ports on zipcode', ylab='Amount of companies')

ports_zipcode_answer <- chisq.test(ports_zipcode_distribution[1:9,c("0","1")], correct = TRUE)
# Warning message:
#   In chisq.test(ports_zipcode_distribution[1:9, c("0", "1")], correct = TRUE) :
#   Chi-squared approximation may be incorrect

View(ports_zipcode_answer$expected)

ports_zipcode_answer

# Pearson's Chi-squared test
# 
# data:  ports_zipcode_distribution[1:9, c("0", "1")]
# X-squared = 7.7596, df = 8, p-value = 0.4573

# Size and Sector
ports_size_sector_distribution <- sqldf("SELECT company_size, name_en, no_open_ports, count(no_open_ports) as amount
                                        FROM organizations_subscriptions
                                        INNER JOIN lightscan ON organizations_subscriptions.id = lightscan.id_cms_organizations_subscriptions
                                        INNER JOIN scans ON organizations_subscriptions.id = scans.id_organizations_subscriptions
                                        INNER JOIN cyberstatus_branches ON lightscan.branche = cyberstatus_branches.id
                                        GROUP BY company_size, name_en,  no_open_ports")

ports_size_sector_distribution <- unite(ports_size_sector_distribution, company_info, company_size:name_en, sep=' : ')

ports_size_sector_distribution <- pivot_wider(ports_size_sector_distribution, names_from = no_open_ports, values_from = amount)

ports_size_sector_distribution[is.na(ports_size_sector_distribution)] <- 0

View(ports_size_sector_distribution)

# ports_size_sector_answer <- chisq.test(ports_size_sector_distribution[1:46,c("0","1")], correct = TRUE)
# Warning message:
# In chisq.test(ports_size_sector_distribution[1:46, c("0", "1")],  :
#   Chi-squared approximation may be incorrect

View(ports_size_sector_answer$expected)

ports_size_sector_answer

# 	Pearson's Chi-squared test
# 
# data:  ports_size_sector_distribution[1:46, c("0", "1")]
# X-squared = 59.685, df = 45, p-value = 0.07019
