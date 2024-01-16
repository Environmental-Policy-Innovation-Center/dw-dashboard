# NOTE: This script was provided by MDI and is kept as an example of how to use the URL / pdfReader approach
# should there be a need to use it instead of the camelot package.

import pandas as pd
import numpy as np
import PyPDF2 
import tabula
import camelot.io as camelot
import ghostscript
import re
import os
import csv
import urllib
import io
import warnings
warnings.filterwarnings("ignore")
## (38) PENNSYLVANIA
# retrieve pdf and read it

URL = "https://files.dep.state.pa.us/Water/BPNPSM/InfrastructureFinance/StateRevolvFundIntendUsePlan/2022/DRINKING_WATER_Federal-FY_2022_PPL_JUN_REV_1.pdf" 
req = urllib.request.Request(URL, headers={'User-Agent' : "Magic Browser"})
remote_file = urllib.request.urlopen(req).read()
remote_file_bytes = io.BytesIO(remote_file)
pdfReader = PyPDF2.PdfFileReader(remote_file_bytes)
pdfWriter = PyPDF2.PdfFileWriter()
count = pdfReader.numPages

# parse the text from the pdf 
pagelist = []
for page in range(1, pdfReader.numPages):
    pageObj = pdfReader.getPage(page)
    pagestring = pageObj.extractText()
    pagelist.append(pagestring)
pagestring = ''.join(pagelist) 
pagestring = pagestring.replace('LEGEND FOR PROJECT TYPE:\nSRC = SOURCE TRANS = TRANSMISSION SYSTEM WS = WATER STORAGE DS = DISTRIBUTION SYSTEM TREAT=TREATMENTPENNSYLVANIA INFRASTRUCTURE INVESTMENT AUTHORITY AND DEPARTMENT OF ENVIRONMENTAL PROTECTION\nDRINKING WATER STATE REVOLVING FUND\nFEDERAL FY2022 - PROJECT PRIORITY LIST\nJUN 2, 2022 REV. 1', '')
 
# extract the values between each variable name 
countymatch = re.findall(r'COUNTY:(.*?)PWS', pagestring, re.DOTALL)
pwsmatch = re.findall(r'PWSID:(.*?)PROJEC', pagestring, re.DOTALL)
costmatch = re.findall(r'PROJECT COST:(.*?)DEP PROJECT', pagestring, re.DOTALL)
depmatch = re.findall(r'DEP PROJECT RATING:(.*?)APPLICANT', pagestring, re.DOTALL)
appmatch = re.findall(r'APPLICANT NAME:(.*?)STREET', pagestring, re.DOTALL)
addressmatch = re.findall(r'ADDRESS:(.*?)CITY:', pagestring, re.DOTALL)
citymatch = re.findall(r'CITY:(.*?)PROJECT TYPE', pagestring, re.DOTALL)
typematch = re.findall(r'PROJECT TYPE:(.*?)PROJRANK', pagestring, re.DOTALL)
rankmatch = re.findall(r'PROJRANK:(.*?)FUND SOURCE:', pagestring, re.DOTALL)
sourcematch = re.findall(r'FUND SOURCE:(.*?)MTGDATE', pagestring, re.DOTALL)
mtgmatch = re.findall(r'MTGDATE:(.*?)REGION:', pagestring, re.DOTALL)
regionmatch = re.findall(r'REGION:(.*?)PROJ. DESC', pagestring, re.DOTALL)
descmatch = re.findall(r'PROJ. DESCRIPTION:(.*?)POPULA', pagestring, re.DOTALL)
probmatch = re.findall(r'PROB. DESCRIPTION:(.*?)POPULA', pagestring, re.DOTALL)
populmatch = re.findall(r'POPULATION:(.*?)PV', pagestring, re.DOTALL)
pvmatch = re.findall(r'PV RATING:(.*?)GREEN CATEGORY', pagestring, re.DOTALL)
gcatmatch = re.findall(r'GREEN CATEGORY:(.*?)GREEN PROJECT', pagestring, re.DOTALL)
gprojmatch = re.findall(r'GREEN PROJECT:(.*?)BUSINESS CASE', pagestring, re.DOTALL)
busmatch = re.findall(r'BUSINESS CASE:(.*?)GREEN AMOUNT', pagestring, re.DOTALL)
gamtmatch = re.findall(r'GREEN AMOUNT:(.*?)\n', pagestring, re.DOTALL)

pa_tbl = pd.DataFrame(
    {'COUNTY': countymatch,
     'PWSID': pwsmatch,
     'PROJECT COST': costmatch,
     'DEP PROJECT RATING': depmatch,
     'APPLICANT': appmatch,
     'STREET ADDRESS': addressmatch,
     'PROJECT TYPE': typematch,
     'CITY': citymatch,
     'PROJRANK': rankmatch,
     'FUND SOURCE': sourcematch,
     'MTGDATE': mtgmatch,
     'REGION': regionmatch,
     'PROJ. DESCRIPTION': descmatch,
     'PROB. DESCRIPTION': probmatch,
     'POPULATION': populmatch,
     'PV RATING': pvmatch,
     'GREEN CATEGORY': gcatmatch,
     'GREEN PROJECT': gprojmatch,
     'BUSINESS CASE': busmatch,
 'GREEN AMOUNT' : gamtmatch
     })
pa_tbl  
# delete blank spaces and quotes from dataframe before saving as csv
pa_tbl.apply(lambda s:s.str.replace('"', ""))
pa_tbl[pa_tbl.columns] = pa_tbl.apply(lambda x: x.str.strip())
pa_tbl.to_csv('38-Pennsylvania_PPL.csv', index=False)