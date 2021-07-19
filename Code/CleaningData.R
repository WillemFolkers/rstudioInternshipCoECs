#Code Data Cleaning
#Lightscan step 1
lightscan <- sqldf("SELECT *
			       FROM lightscan
			       WHERE id_cms_organizations_subscriptions != 0")

#Organizations subscriptions step 1
organizations_subscriptions <- sqldf("SELECT *
						     	   FROM organizations_subscriptions
				       			   WHERE id_lightscan in (SELECT id
						       			   			FROM lightscan)")

organizations_subscriptions <- sqldf("SELECT id, id_organizations, id_subscriptions, id_lightscan, max(date_start) as date_start, status, firewall_blocked, period_scan
		                     			   FROM organizations_subscriptions
		                     			   GROUP BY id_lightscan")
#Scans
scans <- sqldf("SELECT *
	       	         FROM scans
	       	         WHERE id_organizations_subscriptions in (SELECT id
						       	      				    FROM organizations_subscriptions)")

scans <- sqldf("SELECT id, id_organizations, id_organizations_subscriptions, type_scan, max(date) as date, skip_status, invalid_host, no_open_ports, redirect, report_score
	       	         FROM scans
	       	         GROUP BY id_organizations, id_organizations_subscriptions")

#Organizations subscriptions step 2
organizations_subscriptions <- sqldf("SELECT *
						     	   FROM organizations_subscriptions
				       			   WHERE id in (SELECT id_organizations_subscriptions
						       			   	FROM scans)")

#Lightscan step 2
lightscan <- sqldf("SELECT *
			       FROM lightscan
			       WHERE id in (SELECT id_lightscan
    FROM organizations_subscriptions)")

#Lightscan login
Lightscan_login <- sqldf("SELECT *
				       FROM lightscan_login
				       WHERE id in (SELECT id_lightscan_login
				       			    FROM lightscan)")
