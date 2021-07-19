#R/SQL Code Size
size_distribution <- sqldf("SELECT company_size, count(company_size) as amount
                            FROM lightscan
                            GROUP BY company_size")
View(size_distribution)

par(mar=c(6,6,4,4))	
barplot(height=size_distribution$amount, ylim=c(0,2500), names.arg=size_distribution$company_size, main='Distribution of company sizes', col='blue', xlab='Company size', ylab='Amount of companies')

probability <- c(1/2, 1/4, 1/4)
observed <- c(2429, 204, 25)
multinomial.test(observed, probability)

# Exact Multinomial Test, distance measure: p
# 
# Events    pObs    p.value
# 3536470       0          0

#R/SQL Code Branche
branche_distibution <- sqldf("SELECT name_en, count(branche) as amount
                              FROM cyberstatus_branches JOIN lightscan ON cyberstatus_branches.id = lightscan.branche
                              GROUP BY name_en")
View(branche_distibution)

par(mar=c(6,10,4,4))
barplot(height=branche_distibution$amount, xlim=c(0, 350), names.arg=branche_distibution$name_en, cex.names=0.5, las=1, horiz=TRUE, col='blue', main='Distribution of company sectors', xlab='Amount of companies')

probability <- c(1/21, 1/21, 1/21, 1/21, 1/21, 1/21, 1/21, 1/21, 1/21, 1/21, 1/21, 1/21, 1/21, 1/21, 1/21, 1/21, 1/21, 1/21, 1/21, 1/21, 1/21)
observed <- c(45, 10, 267, 280, 261, 100, 65, 27, 211, 22, 123, 122, 99, 39, 35, 78, 306, 110, 19, 81, 358)
multinomial.test(observed, probability)

# Exact Multinomial Test, distance measure: p
# 
# Events    pObs    p.value
# 3536470       0          0

#R/SQL Code Zip Code
zipcode_distribution <- sqldf("SELECT zipcode, count(company_size) as amount
                            FROM lightscan
                            GROUP BY zipcode")
View(zipcode_distribution)

par(mar=c(6,6,4,4))
barplot(height=zipcode_distribution$amount, ylim=c(0,1600), names.arg=zipcode_distribution$zipcode, col='blue', main='Distribution of company zip codes', xlab='Company zip code', ylab='Amount of companies')

probability <- c(1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9, 1/9)
observed <- c(174, 154, 194, 95, 152, 142, 98, 64, 49)
multinomial.test(observed, probability)

# Exact Multinomial Test, distance measure: p
# 
# Events    pObs    p.value
# 3536470       0          0
