# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np

def MonthConcat(symbol, interval, base_frame, years, months):
    frame = pd.DataFrame(None, columns = ['Open_Time', 'Open', 'High', 'Low', 'Close', 'Volume', 'Close_Time', 'Quote_Asset_Vol', 'No. Trades', 'Taker Buy Base Vol', 'Taker Buy Quote Vol', 'Ignore'])
    for i in range(len(years)):
        for k in range(len(months)):
            file_name = symbol + '-' + interval + '-' + str(years[i]) + '-' + months[k] + '.csv'
            try:
                new_data = pd.read_csv(file_name)
            except FileNotFoundError:
                print(file_name, 'is not found')
            else:
                print(file_name, 'is found')
                print(len(new_data), 'rows')
                new_data.columns = ['Open_Time', 'Open', 'High', 'Low', 'Close', 'Volume', 'Close_Time', 'Quote_Asset_Vol', 'No. Trades', 'Taker Buy Base Vol', 'Taker Buy Quote Vol', 'Ignore']
                frame = frame.append(new_data)
                current_date = str(years[i]) + '-' + months[k]
    data = base_frame.append(frame)            
    return data, current_date

def DailyConcat(symbol, interval, base_frame, date_range):
    frame = pd.DataFrame(None, columns = ['Open_Time', 'Open', 'High', 'Low', 'Close', 'Volume', 'Close_Time', 'Quote_Asset_Vol', 'No. Trades', 'Taker Buy Base Vol', 'Taker Buy Quote Vol', 'Ignore'])
    for i in range(len(date_range)):
        file_name = symbol + '-' + interval + '-' + str(date_range[i].date()) + '.csv'
        try:
            new_data = pd.read_csv(file_name)
        except FileNotFoundError:
            print(file_name, 'is not found')
        else:
            print(file_name, 'is found')
            print(len(new_data), 'rows')
            new_data.columns = ['Open_Time', 'Open', 'High', 'Low', 'Close', 'Volume', 'Close_Time', 'Quote_Asset_Vol', 'No. Trades', 'Taker Buy Base Vol', 'Taker Buy Quote Vol', 'Ignore']
            frame = frame.append(new_data)
            current_date = str(date_range[i].date())
    data = base_frame.append(frame)            
    return data, current_date

# This returns a pandas dataframe

def CompleteConcat(symbol, interval, base_frame, years, months, date_range, writecsv = False, monthly = True, daily = True):
    if monthly == True and daily == True:
        data, current_date_monthly = MonthConcat(symbol, interval, base_frame, years, months)
        data_full, current_date_daily = DailyConcat(symbol, interval, base_frame = data, date_range = date_range)
        if writecsv == True:
            data_full.to_csv(symbol + '_' + current_date_daily +'.csv', index = False)
    elif monthly == False:
        data_full, current_date_daily = DailyConcat(symbol, interval, base_frame = base_frame, date_range = date_range)
        if writecsv == True:
            data_full.to_csv(symbol + '_' + current_date_daily +'.csv', index = False)
    elif daily == False:
        data_full, current_date_monthly = MonthConcat(symbol, interval, base_frame, years, months)
        if writecsv == True:
            data_full.to_csv(symbol + '_' + current_date_monthly +'.csv', index = False)
    return data_full

if __name__ == "__main__":

    # Coin symbol
    symbol = 'BTCUSDT'
    # How often the data is collected
    interval = '1m'
    # First year of monthly data
    first_year = 2017
    # Year after last year of monthly data
    next_year = 2022
    # Start date for Daily data
    start_date = '2021-12-01'
    # End date for daily data
    end_date = '2021-12-31'
    # Variables in the csv files that are getting read
    variables = ['Open_Time', 'Open', 'High', 'Low', 'Close', 'Volume', 'Close_Time', 'Quote_Asset_Vol', 'No. Trades', 'Taker Buy Base Vol', 'Taker Buy Quote Vol', 'Ignore']
    # Base dataframe to append to, is empty here as we are starting from no data however if adding new data we use the already created frame
    base_frame = pd.DataFrame(None, columns = variables)
    
    # Iterables for file names
    years = np.arange(first_year,next_year)
    months = [str(x).zfill(2) for x in np.arange(1,13)]
    date_range = pd.date_range(start = start_date, end = end_date)   
    
    ## BTC 1st Concat
    
    symbol = 'BTCUSDT'
    BTC_data = CompleteConcat(symbol, interval, base_frame, years, months, date_range, writecsv = True)
    
    
    ## Eth 1st Concat
    
    symbol = 'ETHUSDT'
    ETH_data = CompleteConcat(symbol, interval, base_frame, years, months, date_range, writecsv = True)   




