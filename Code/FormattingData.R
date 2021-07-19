#Code Formatting Zip Code
#Lightscan_login
lightscan_login <- sqldf(c("UPDATE lightscan_login
                     SET zip = '1000AA-1999ZZ'
                     WHERE zip LIKE '1%'",
                           "SELECT *
                     FROM lightscan_login "))

lightscan_login <- sqldf(c("UPDATE lightscan_login
                     SET zip = '2000AA-2999ZZ'
                     WHERE zip LIKE '2%'
   		   OR zip = 'Pr. Beatrixlaan 582'",
                           "SELECT *
                     FROM lightscan_login "))

lightscan_login <- sqldf(c("UPDATE lightscan_login
                     SET zip = '3000AA-3999ZZ'
                     WHERE zip LIKE '3%'",
                           "SELECT *
                     FROM lightscan_login "))

lightscan_login <- sqldf(c("UPDATE lightscan_login
                     SET zip = '4000AA-4999ZZ'
                     WHERE zip LIKE '4%'",
                           "SELECT *
                     FROM lightscan_login "))

lightscan_login <- sqldf(c("UPDATE lightscan_login
                     SET zip = '5000AA-5999ZZ'
                     WHERE zip LIKE '5%'",
                           "SELECT *
                     FROM lightscan_login "))

lightscan_login <- sqldf(c("UPDATE lightscan_login
                     SET zip = '6000AA-6999ZZ'
                     WHERE zip LIKE '6%'",
                           "SELECT *
                     FROM lightscan_login "))

lightscan_login <- sqldf(c("UPDATE lightscan_login
                     SET zip = '7000AA-7999ZZ'
                     WHERE zip LIKE '7%'",
                           "SELECT *
                     FROM lightscan_login "))

lightscan_login <- sqldf(c("UPDATE lightscan_login
                     SET zip = '8000AA-8999ZZ'
                     WHERE zip LIKE '8%'",
                           "SELECT *
                     FROM lightscan_login "))

lightscan_login <- sqldf(c("UPDATE lightscan_login
                     SET zip = '9000AA-9999ZZ'
                     WHERE zip LIKE '9%'",
                           "SELECT *
                     FROM lightscan_login "))

lightscan_login <- sqldf(c("UPDATE lightscan_login
                     SET zip = 'N/A'
                     WHERE zip = ''
                     OR zip = 'EMPTY_ZIP'",
                           "SELECT *
                     FROM lightscan_login "))

#Ligthscan
lightscan <- sqldf(c("UPDATE lightscan
                     SET zipcode = '1000AA-1999ZZ'
                     WHERE zipcode LIKE '1%'",
                     "SELECT *
                     FROM lightscan"))

lightscan <- sqldf(c("UPDATE lightscan
                     SET zipcode = '2000AA-2999ZZ'
                     WHERE zipcode LIKE '2%'
   OR zipcode = 'Pr. Beatrixlaan 582'",
                     "SELECT *
                     FROM lightscan"))

lightscan <- sqldf(c("UPDATE lightscan
                     SET zipcode = '3000AA-3999ZZ'
                     WHERE zipcode LIKE '3%'",
                     "SELECT *
                     FROM lightscan"))

lightscan <- sqldf(c("UPDATE lightscan
                     SET zipcode = '4000AA-4999ZZ'
                     WHERE zipcode LIKE '4%'",
                     "SELECT *
                     FROM lightscan"))

lightscan <- sqldf(c("UPDATE lightscan
                     SET zipcode = '5000AA-5999ZZ'
                     WHERE zipcode LIKE '5%'",
                     "SELECT *
                     FROM lightscan"))

lightscan <- sqldf(c("UPDATE lightscan
                     SET zipcode = '6000AA-6999ZZ'
                     WHERE zipcode LIKE '6%'",
                     "SELECT *
                     FROM lightscan"))

lightscan <- sqldf(c("UPDATE lightscan
                     SET zipcode = '7000AA-7999ZZ'
                     WHERE zipcode LIKE '7%'",
                     "SELECT *
                     FROM lightscan"))

lightscan <- sqldf(c("UPDATE lightscan
                     SET zipcode = '8000AA-8999ZZ'
                     WHERE zipcode LIKE '8%'",
                     "SELECT *
                     FROM lightscan"))

lightscan <- sqldf(c("UPDATE lightscan
                     SET zipcode = '9000AA-9999ZZ'
                     WHERE zipcode LIKE '9%'",
                     "SELECT *
                     FROM lightscan"))

lightscan <- sqldf(c("UPDATE lightscan
                     SET zipcode = 'N/A'
                     WHERE zipcode = ''
                     OR zipcode = 'EMPTY_ZIP'",
                     "SELECT *
                     FROM lightscan"))

#Code Formating Company Size
#Lightscan_login
lightscan_login <- sqldf(c("UPDATE lightscan_login
                     SET company_size = '0-50'
                     WHERE company_size = 'ZZP'
                     OR company_size = 'zpp'
                     OR company_size = 'zzp'
                     OR company_size = '-10'
                     OR company_size = '-50'",
                           "SELECT *
                     FROM lightscan_login"))

lightscan_login <- sqldf(c("UPDATE lightscan_login
                     SET company_size = '50-250'
                     WHERE company_size = '-100'
                     OR company_size = '-250'",
                           "SELECT *
                     FROM lightscan_login"))

#Ligthscan
lightscan <- sqldf(c("UPDATE lightscan
                     SET company_size = '0-50'
                     WHERE company_size = 'ZZP'
                     OR company_size = 'zpp'
                     OR company_size = 'zzp'
                     OR company_size = '-10'
                     OR company_size = '-50'",
                     "SELECT *
                     FROM lightscan"))

lightscan <- sqldf(c("UPDATE lightscan
                     SET company_size = '50-250'
                     WHERE company_size = '-100'
                     OR company_size = '-250'",
                     "SELECT *
                     FROM lightscan"))

lightscan <- sqldf(c("UPDATE lightscan
                     SET company_size = '>250'
                     WHERE company_size = '+250'",
                     "SELECT *
                     FROM lightscan"))

#Code Formating Period Scan
organizations_subscriptions <- sqldf(c("UPDATE organizations_subscriptions
                                       SET period_scan = 'N/A'
                                       WHERE period_scan = ''",
                                       "SELECT *
                                       FROM organizations_subscriptions"))

#Code Formatting Report Score
scans <- sqldf(c("UPDATE scans
                     	   SET report_score = 1
                     	   WHERE report_score = -1",
                 "SELECT *
                     	   FROM scans"))
