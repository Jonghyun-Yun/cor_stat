import pandas as pd
data = pd.io.stata.read_stata('01_Socio-Geographic Factor Impacts.dta')
data.to_csv('01_Socio-Geographic Factor Impacts.csv')
