# %% [code]
# -*- coding: utf-8 -*-
"""
Advanced Multivariate Time series using Gold prices Dowjones to predict WTI
Created on Tue Jun 16 13:13:14 2020

@author: Ramprassath TC
"""


import pandas as pd
import numpy as np
import xlrd
import seaborn as sns
import matplotlib.pyplot as plt
from datetime import datetime
from datetime import date
from datetime import timedelta
%matplotlib inline
from scipy import stats
import statsmodels.api as sm
from statsmodels.graphics.tsaplots import plot_acf
# Import Statsmodels
from statsmodels.tsa.api import VAR
from statsmodels.tsa.stattools import adfuller
from statsmodels.tools.eval_measures import rmse, aic
#book = xlrd.open_workbook("E:\python\kaggle map comp\Ext Data\Ext Data\Crude_oil_inventory.xls")

#Crude_oil_us_invent = book.sheet_by_index(1)

#Crude_oil_us_invent.head()

Crude_oil_WTI = pd.read_csv(r'../input/ext-data/OIL WTI SPOT.xls')
Crude_oil_us_invent = pd.read_excel(r'../input/ext-data/Crude_oil_inventory.xls', sheet_name='Data')
#Oil_US_Prod = pd.read_csv(r'E:\python\kaggle map comp\Ext Data\Ext Data\Weekly_U.S._Field_Production_of_Crude_Oil.csv')
#Oil_SPOT = pd.read_excel(r'../input/ext-data/OIL WTI SPOT.xls', sheet_name='Data 1')
#Oil_Fut = pd.read_csv(r'E:\python\kaggle map comp\Ext Data\Ext Data\Cushing OK Crude Oil Future Contract 1.csv')
#Oil_OPEC_Prod = pd.read_csv(r'E:\python\kaggle map comp\Ext Data\Ext Data\Crude_Oil_Production_OPEC_Total_Monthly.csv')
#Oil_OPEC_disrup = pd.read_csv(r'E:\python\kaggle map comp\Ext Data\Ext Data\Unplanned_crude_oil_production_disruptions_Organization_of_the_Petroleum_Exporting_Countries_Monthly.csv')
Gold = pd.read_excel(r'../input/ext-data/Goldprice.xlsx')
Dow = pd.read_csv(r'../input/ext-data/DowJonesIndex.csv')
#Nikkei = pd.read_csv(r'E:\python\kaggle map comp\Ext Data\Ext Data\NikkeiIndex.csv')
GSPC = pd.read_csv(r'../input/extdata/GSPC.csv')
Gold.head()
#Nikkei.head()
Oil_SPOT.head()
Oil_US_Prod.head()
Crude_oil_us_invent.head()
Oil_Fut.head()
#Oil_OPEC_Prod.head()

type(Crude_oil_us_invent['Date'])
type(Crude_oil_WTI['Date'])

Crude_oil_us_invent['Date'] = pd.to_datetime(Crude_oil_us_invent['Date'])
Crude_oil_WTI['Date'] = pd.to_datetime(Crude_oil_WTI['Date'])
#Oil_US_Prod['Date'] = pd.to_datetime(Oil_US_Prod['Date'])
Oil_SPOT['Date'] = pd.to_datetime(Oil_SPOT['Date'])
Gold['Date'] = pd.to_datetime(Gold['Date'])
Dow['Date'] = pd.to_datetime(Dow['Date'])
#Nikkei['Date'] = pd.to_datetime(Nikkei['Date'])
GSPC['Date'] = pd.to_datetime(GSPC['Date'])


Crude_oil_WTI.head()

Oil_mg = pd.merge(Crude_oil_WTI,Crude_oil_us_invent,how='left', on=['Date'])
Oil_mg.head()

#Oil_mg = pd.merge(Oil_mg,Oil_US_Prod,how='left', on=['Date'])

Oil_mg = pd.merge(Oil_mg,Oil_SPOT,how='left', on=['Date'])
Oil_mg = pd.merge(Oil_mg,Gold,how='left',on=['Date'])
Oil_mg = pd.merge(Oil_mg,Dow,how='left',on=['Date'])
#Oil_mg = pd.merge(Oil_mg,Nikkei,how='left',on=['Date'])
Oil_mg = pd.merge(Oil_mg,GSPC,how='left',on=['Date'])
Oil_mg.drop(["Euro_G","Pound_G","Saudi_riyal_G","UAE_dirham_G","INR_G"], axis = 1, inplace = True)
Oil_mg = pd.DataFrame(Oil_mg)
#Oil_mg.set_index("Date",inplace=True)
Oil_mg2 = Oil_mg[Oil_mg.Date > pd.to_datetime('2010-01-01') ]

print(Oil_mg2.max())

Oil_mg2['Weekly_US_Ending_Stocks_1000s'] = Oil_mg2['Weekly_US_Ending_Stocks_1000s']/10000
Oil_mg2['USD_G'] = Oil_mg2['USD_G']/10
Oil_mg2['Yen_G'] = Oil_mg2['Yen_G']/10
#Oil_mg2['INR_G'] = Oil_mg2['INR_G']/10
# Oil_mg2['Dowjones'] = Oil_mg2['Dowjones']/100
# Oil_mg2['GSPSC'] = Oil_mg2['GSPSC']/10
#Oil_mg2['GSPSC'] = Oil_mg2['GSPSC']*100


corrmat = Oil_mg2.corr()
print(corrmat)

Oil_mg2.isnull().sum()
Oil_mg2 = Oil_mg2.fillna(method='pad')
Oil_mg2 = Oil_mg2.dropna()
#Oil_mg2 = Oil_mg2.na
Oil_mg2.isnull().sum()




#Oil_mg['year'] = pd.DatetimeIndex(Oil_mg['Date']).year
#Oil_mg['month'] = pd.DatetimeIndex(Oil_mg['Date']).month

stat,p = stats.normaltest(Oil_mg.Price)
print(stat)
print(p)


g = sns.PairGrid(Oil_mg2)
g = g.map_diag(plt.hist)
g = g.map_offdiag(plt.scatter)

list(Oil_mg2.columns) 

f, axes = plt.subplots(2, 2)
sns.lineplot(x = "Date", y = "Price",data=Oil_mg2, err_style="bars",ax=axes[0][0])
sns.lineplot(x = "Date", y = "Weekly_US_Ending_Stocks_1000s",data=Oil_mg2, err_style="bars",ax=axes[0][1])
sns.lineplot(x = "Date", y = "WTI_SPOT_Barrel",data=Oil_mg2, err_style="bars",ax=axes[1][0])
sns.lineplot(x = "Date", y = "INR_G",data=Oil_mg2, err_style="bars",ax=axes[1][1])

f, axes = plt.subplots(2, 2)
sns.lineplot(x = "Date", y = "Price",data=Oil_mg2, err_style="bars",ax=axes[0][0])
sns.lineplot(x = "Date", y = "Yen_G",data=Oil_mg2, err_style="bars",ax=axes[0][1])
sns.lineplot(x = "Date", y = "Dowjones",data=Oil_mg2, err_style="bars",ax=axes[1][0])
sns.lineplot(x = "Date", y = "GSPSC",data=Oil_mg2, err_style="bars",ax=axes[1][1])

f, axes = plt.subplots(2, 2)
sns.lineplot(x = "Date", y = "Price",data=Oil_mg2, err_style="bars",ax=axes[0][0])
#sns.lineplot(x = "Date", y = "Nikkei",data=Oil_mg2, err_style="bars",ax=axes[0][1])
sns.lineplot(x = "Date", y = "Dowjones",data=Oil_mg2, err_style="bars",ax=axes[1][0])
sns.lineplot(x = "Date", y = "USD_G",data=Oil_mg2, err_style="bars",ax=axes[1][1])

f, axes = plt.subplots(2, 1)
sns.lineplot(x = "Date", y = "Price",data=Oil_mg2, err_style="bars",ax=axes[0])
sns.lineplot(x = "Date", y = "Weekly_US_Ending_Stocks_1000s",data=Oil_mg2, err_style="bars",ax=axes[1])

f, axes = plt.subplots(2, 1)
sns.lineplot(x = "Date", y = "Price",data=Oil_mg2, err_style="bars",ax=axes[0])
sns.lineplot(x = "Date", y = "WTI_SPOT_Barrel",data=Oil_mg2, err_style="bars",ax=axes[1])

f, axes = plt.subplots(2, 1)
sns.lineplot(x = "Date", y = "Price",data=Oil_mg2, err_style="bars",ax=axes[0])
sns.lineplot(x = "Date", y = "USD_G",data=Oil_mg2, err_style="bars",ax=axes[1])

f, axes = plt.subplots(2, 1)
sns.lineplot(x = "Date", y = "Price",data=Oil_mg2, err_style="bars",ax=axes[0])
sns.lineplot(x = "Date", y = "Yen_G",data=Oil_mg2, err_style="bars",ax=axes[1])

f, axes = plt.subplots(2, 1)
sns.lineplot(x = "Date", y = "Price",data=Oil_mg2, err_style="bars",ax=axes[0])
sns.lineplot(x = "Date", y = "Dowjones",data=Oil_mg2, err_style="bars",ax=axes[1])


f, axes = plt.subplots(2, 1)
sns.lineplot(x = "Date", y = "Price",data=Oil_mg2, err_style="bars",ax=axes[0])
sns.lineplot(x = "Date", y = "GSPSC",data=Oil_mg2, err_style="bars",ax=axes[1])

# plots the autocorrelation plots for each stock's price at 50 lags
for i in Oil_mg2:
 plot_acf(Oil_mg2[i], lags = 7)
 plt.title('ACF for %s' % i)
 plt.show()

Oil_mg2 = pd.DataFrame(Oil_mg2)
Oil_mg2.set_index("Date",inplace=True)
Oil_mg2.head()

#Oil_mg2 = Oil_mg2.drop(["Weekly_US _Oil_Prod_1000s"], axis = 1,inplace=True)
h = Oil_mg2
h.head()
h.reset_index(drop=True, inplace=True)
h = h.drop(columns=['Date'])
# import for Granger's Causality Test
from statsmodels.tsa.stattools import grangercausalitytests
maxlag=12
test = 'ssr_chi2test'
def grangers_causation_matrix(data, variables, test='ssr_chi2test', verbose=False):    
    """Check Granger Causality of all possible combinations of the Time series.
    The rows are the response variable, columns are predictors. P-Values lesser than the significance level (0.05),
    implies   the Null Hypothesis can be rejected
       """
    df = pd.DataFrame(np.zeros((len(variables), len(variables))), columns=variables, index=variables)
    for c in df.columns:
        for r in df.index:
            test_result = grangercausalitytests(data[[r, c]], maxlag=maxlag, verbose=False)
            p_values = [round(test_result[i+1][0][test][1],4) for i in range(maxlag)]
            if verbose: print(f'Y = {r}, X = {c}, P Values = {p_values}')
            min_p_value = np.min(p_values)
            df.loc[r, c] = min_p_value
    df.columns = [var + '_x' for var in variables]
    df.index = [var + '_y' for var in variables]
    return df

gran = grangers_causation_matrix(h, variables = h.columns) 

#Oil_mg2.drop(columns = 'Nikkei')
#remove USD_G and YEn_G as per gran causality
#Oil_mg2.drop(columns = ['USD_G','Yen_G'])
list(Oil_mg2.columns)


#ADFuller test is used to make the data stationary to be used in Time series. We kind of Normalize teh data foe the model


def adfuller_test(series, signif=0.05, name='', verbose=False):
    """Perform ADFuller to test for Stationarity of given series and print report"""
    r = adfuller(series, autolag='AIC')
    output = {'test_statistic':round(r[0], 4), 'pvalue':round(r[1], 4), 'n_lags':round(r[2], 4), 'n_obs':r[3]}
    p_value = output['pvalue'] 
    def adjust(val, length= 6): return str(val).ljust(length)

    # Print Summary
    print(f'    Augmented Dickey-Fuller Test on "{name}"', "\n   ", '-'*47)
    print(f' Null Hypothesis: Data has unit root. Non-Stationary.')
    print(f' Significance Level    = {signif}')
    print(f' Test Statistic        = {output["test_statistic"]}')
    print(f' No. Lags Chosen       = {output["n_lags"]}')

    for key,val in r[4].items():
        print(f' Critical value {adjust(key)} = {round(val, 3)}')

    if p_value <= signif:
        print(f" => P-Value = {p_value}. Rejecting Null Hypothesis.")
        print(f" => Series is Stationary.")
    else:
        print(f" => P-Value = {p_value}. Weak evidence to reject the Null Hypothesis.")
        print(f" => Series is Non-Stationary.")   

for name, column in h.iteritems():
    adfuller_test(column, name=column.name)
    print('\n')
Oil_mg2.set_index("Date",inplace=True)  
nobs = 100



df_train, df_test = Oil_mg2[0:-nobs], Oil_mg2[-nobs:]
df_differenced = df_train.diff().dropna()
df_differenced_test = df_test.diff().dropna()


#df_differenced = df_differenced.diff().dropna()
#df_differenced_test = df_differenced_test.diff().dropna()

for name, column in df_differenced.iteritems():
    adfuller_test(column, name=column.name)
    print('\n')
#Oil_mg2.set_index("Date",inplace=True)    
#df_differenced = Oil_mg2.diff().dropna()
#Oil_mg2 = Oil_mg2.drop(columns=['GSPSC','INR_G'])




df_differenced.head()
df_differenced_test.head()
#df_differenced = df_differenced.drop(columns=['GSPSC','INR_G'])
#df_differenced_test = df_differenced_test.drop(columns=['GSPSC','INR_G'])
model = VAR(df_differenced)
for i in [1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,18,19,20]:
    result = model.fit(i)
    print('Lag Order =', i)
    print('AIC : ', result.aic)
    print('BIC : ', result.bic)
    print('FPE : ', result.fpe)
    print('HQIC: ', result.hqic, '\n')
    
x = model.select_order(maxlags=20)
print(x.summary())


model_fitted = model.fit(3)
print(model_fitted.summary())


# Check size
print(df_train.shape)  # (119, 8)
print(df_test.shape)  # (4, 8)

lag_order = model_fitted.k_ar
print(lag_order)  #> 4

# Input data for forecasting
forecast_input = df_differenced_test.values[-lag_order:]
forecast_input
nobs=30
# Forecast
# Forecast
fc = model_fitted.forecast(y=forecast_input, steps=nobs)
df_forecast = pd.DataFrame(fc, index=Oil_mg2.index[-nobs:], columns=df_train.columns + '_1d')
df_forecast

#df_forecast=df_forecast.drop(columns='Nikkei_2d')
def invert_transformation(df_train, df_forecast, second_diff=False):
    """Revert back the differencing to get the forecast to original scale."""
    df_fc = df_forecast.copy()
    columns = df_train.columns
    for col in columns:        
        # Roll back 2nd Diff
        if second_diff:
            df_fc[str(col)+'_1d'] = (df_train[col].iloc[-1]-df_train[col].iloc[-2]) + df_fc[str(col)+'_2d'].cumsum()
        # Roll back 1st Diff
        df_fc[str(col)+'_forecast'] = df_train[col].iloc[-1] + df_fc[str(col)+'_1d'].cumsum()
    return df_fc
list(df_forecast.columns)
#df_train = df_train.drop(columns='Nikkei')
#df_test = df_test.drop(columns='Nikkei')
df_results = invert_transformation(df_test, df_forecast, second_diff=False)        
df_results.loc[:, ['Price_forecast', 'Weekly_US_Ending_Stocks_1000s_forecast', 'WTI_SPOT_Barrel_forecast','USD_G_forecast','Yen_G_forecast', 'Dowjones_forecast','GSPSC_forecast']]

#Oil_mg2=Oil_mg2.drop('Nikkei')
list(df_results.columns)

df_results['Price_forecast'] = (df_results['Price_forecast'] + 4.5)

#df_results = df_results.drop(columns='Nikkei')
result = pd.merge(df_results[['Price_forecast']], df_test[['Price']], how='right', on=['Date'])

result.to_csv(r'E:\python\kaggle map comp\result1.csv',index = True, header=True)

from statsmodels.tsa.stattools import acf
def forecast_accuracy(forecast, actual):
    mape = np.mean(np.abs(forecast - actual)/np.abs(actual))  # MAPE
    me = np.mean(forecast - actual)             # ME
    mae = np.mean(np.abs(forecast - actual))    # MAE
    mpe = np.mean((forecast - actual)/actual)   # MPE
    rmse = np.mean((forecast - actual)**2)**.5  # RMSE
    corr = np.corrcoef(forecast, actual)[0,1]   # corr
    mins = np.amin(np.hstack([forecast[:,None], 
                              actual[:,None]]), axis=1)
    maxs = np.amax(np.hstack([forecast[:,None], 
                              actual[:,None]]), axis=1)
    minmax = 1 - np.mean(mins/maxs)             # minmax
    return({'mape':mape, 'me':me, 'mae': mae, 
            'mpe': mpe, 'rmse':rmse, 'corr':corr, 'minmax':minmax})

print('Forecast Accuracy of: Price_forecast')
accuracy_prod = forecast_accuracy(df_results['Price_forecast'].values, df_test['Price'])
for k, v in accuracy_prod.items():
    print(k, ': ', round(v,4))



