# -*- coding: utf-8 -*-
"""
Created on Thu Apr  9 14:00:13 2020

@author: cstef
"""

import twint

c = twint.Config()
c.Search = "#Oscars2020" or "#Oscars" or "#AcademyAwards" or "#academyawards2020"
# c.Limit = 10000
c.Lang = "en"
c.Since = "2020-01-20"
c.Until = "2020-02-20"  
c.Store_csv = True
c.Output = "D:\\Msc_AI_UoM\\Semester 2\\Text mining\\cw02\\my_data.csv"
# Run
twint.run.Search(c)