# Assignmnet 4.3
# James Taylor

DROP DATABASE if exists my_zip_codes;	
CREATE DATABASE my_zip_codes;
USE my_zip_codes;

# I used Import Wizard to import the CSV

SELECT city, ',' , zip, ',' , tourist_rating FROM zip_code_data   	# this creates the CSV with commas
WHERE tourist_rating >= 4											# seperating the values
ORDER BY tourist_rating DESC;