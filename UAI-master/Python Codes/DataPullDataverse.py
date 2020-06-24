# -*- coding: utf-8 -*-
"""
Created on Wed Jun 27 21:21:24 2018

@author: moniy
"""
from dataverse import Connection

host = 'dataverse.harvard.edu'                  # All clients >4.0 are supported
token = 'ed0b265c-8c6b-417a-883c-cd077d7ae354'  # Generated at /account/apitoken
connection = Connection(host, token)
dataverse = connection.get_dataverse() ### tHIS IS THE PROBLEM
dataset = dataverse.get_dataset_by_doi('DOI:10.7910/DVN/LAYMOS')
files = dataset.get_files('latest')


### DOESNT WORK. THIS IS WHY I CALLED YOU. iT WORKS ON r
##inVALID CREDENTIALS ERROR

##sAME CREDS WORK ON r
tHE GUY WHO IS RPRASAD ON SLACK GROUP
HE WROTE IT