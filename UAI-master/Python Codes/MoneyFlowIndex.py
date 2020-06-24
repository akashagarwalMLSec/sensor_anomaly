# -*- coding: utf-8 -*-
"""
Created on Thu Dec 20 15:04:51 2018
@author: moniy
"""

import pandas as pd
import numpy as np
import os



def MoneyFlowIndex(filename,n):
    df = pd.read_csv(filename)
    typical_price = (df['High'] + df['Low'] + df['Close']) / 3
    money_flow =  df['Typical Price'] * df['Volume']

    Negative_Money_flow = []
    positive_Money_flow = []

    Negative_Money_flow.append(0)
    positive_Money_flow.append(0)


    for row in range(len(typical_price)-1):

        if(typical_price[row] < typical_price[row+1]):
            positive_Money_flow.append(money_flow[row+1])
            Negative_Money_flow.append(0)
        elif(typical_price[row] > typical_price[row+1]):
            positive_Money_flow.append(0)
            Negative_Money_flow.append(money_flow[row+1])
        else:
            Negative_Money_flow.append(0)
            positive_Money_flow.append(0)



    rollinneg = np.append(np.repeat(np.nan, n),np.convolve(Negative_Money_flow,np.ones(n+1,dtype=int),'valid'))

    rollinpos = np.append(np.repeat(np.nan, n),np.convolve(positive_Money_flow,np.ones(n+1,dtype=int),'valid'))

    money_ratio = rollinpos / rollinneg
    money_flow = 100 * (money_ratio / (1+money_ratio))

    df_new = np.concatenate((np.array(typical_price).reshape(len(df),1),np.array(positive_Money_flow).reshape(len(df),1),np.array(Negative_Money_flow).reshape(len(df),1),rollinpos.reshape(rollinpos.shape[0],1),rollinneg.reshape(rollinneg.shape[0],1),money_flow.reshape(money_flow.shape[0],1)), axis=1)

    df_oldcols = df.iloc[:,list(range(6))]
    df_newcols = pd.DataFrame(df_new,columns=['Typical Price','Positive Money Flow','Negative Money Flow','Positive Money Flow','Negative Money Flow','Money Flow Index'])
    df_final = pd.concat((df_oldcols,df_newcols),axis=1)

    name = "money_flow_index[" + str(n) + "].csv"
    df_final.to_csv(name)

def main():
    os.chdir("C:\\Users\\moniy\\Desktop")
    filename = str(input())
    window_size = int(input())

    MoneyFlowIndex(filename,window_size)


if __name__ == "__main__":
    main()


