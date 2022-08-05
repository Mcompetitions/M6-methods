###########################################################################
#  Code for computing the RPS and IR scores for a given evaluation period
###########################################################################

#For simplicity, in this example it is assumed that the data provided cover a single evaluation period.
#This period is specified through the min/max date of the asset prices data set.
#If you wish to compute RPS/IR for multiple periods, you'll have to execute 
#the script multiple times, each time using a different, appropriate input.

import pandas as pd
import numpy as np
from statistics import stdev


#Read asset prices data (as provided by the M6 submission platform)
asset_data = pd.read_csv("assets_m6.csv")

#Read submission file (similar to the template provided by the M6 submission platform)
submission_data = pd.read_csv("template.csv")

def ffill_missing_prices(hist_data):
    pivoted = hist_data.pivot_table(index=['date'], columns=['symbol'], values=['price']).fillna(method='ffill', axis=0)
    pivoted.columns = pivoted.columns.get_level_values(1)
    pivoted = pivoted.reset_index()
    hist_data = pivoted.melt(id_vars=['date'], value_name='price')
    return hist_data

hist_data = ffill_missing_prices(asset_data)
submission = submission_data



#Function for computing RPS
def RPS_calculation(hist_data, submission, asset_no=100):
    
    if hist_data.shape[0]<=asset_no:
        return np.nan

    asset_id = pd.unique(hist_data.symbol)

    #Compute percentage returns
    asset_id = sorted(asset_id)

    returns = hist_data.groupby('symbol')['price'].agg(('last', 'first')).pipe(lambda df: (df['last']-df['first'])/df['first']).rename_axis(index='ID').to_frame(name='Return').reset_index()

    #Define the relevant position of each asset
    ranking = pd.DataFrame(columns=["ID", "Position", "Return"])
    ranking.ID = list(asset_id)
    ranking.Return = returns.Return
    ranking.Position = ranking.Return.rank(method = 'min')

    #Handle Ties
    Series_per_position = pd.DataFrame(columns=["Position","Series", "Rank", "Rank1", "Rank2","Rank3", "Rank4", "Rank5"])
    Series_per_position.Position = list(pd.unique(ranking.Position.sort_values(ascending=True)))

    if len(Series_per_position) == len(asset_id):
        # no ties, use fast code
        returns['Rank'] = pd.qcut(returns['Return'], q=[0, .2, .4, .6, .8, 1]).cat.codes + 1
        ranking =  pd.concat([ranking, returns['Rank'], pd.get_dummies(returns['Rank'], prefix='Rank').rename(columns=lambda x: x.replace('_', '')).astype(float)], axis=1)
    else:
        temp = ranking.Position.value_counts()
        temp = pd.DataFrame(zip(temp.index, temp), columns = ["Rank", "Occurencies"])
        temp = temp.sort_values(by = ["Rank"],ascending=True)
        Series_per_position.Series = list(temp.Occurencies)
        Series_per_position

        total_ranks = Series_per_position.Position.values[-1]
        for i in range(0,Series_per_position.shape[0]):

            start_p = Series_per_position.Position[i]
            end_p = Series_per_position.Position[i] + Series_per_position.Series[i]
            temp = pd.DataFrame(columns = ["Position","Rank", "Rank1", "Rank2", "Rank3", "Rank4","Rank5"])
            temp.Position = list(range(int(start_p),int(end_p)))

            if(temp.loc[temp.Position.isin(list(range(1,int(0.2*total_ranks+1))))].empty==False):
                temp.loc[temp.Position.isin(list(range(1,int(0.2*total_ranks+1))))] = temp.loc[temp.Position.isin(list(range(1,int(0.2*total_ranks+1))))].assign(Rank=1)
                temp.loc[temp.Position.isin(list(range(1,int(0.2*total_ranks+1))))] = temp.loc[temp.Position.isin(list(range(1,int(0.2*total_ranks+1))))].assign(Rank1=1.0)

            elif(temp.loc[temp.Position.isin(list(range(int(0.2*total_ranks+1),int(0.4*total_ranks+1))))].empty==False):
                temp.loc[temp.Position.isin(list(range(int(0.2*total_ranks+1),int(0.4*total_ranks+1))))] = temp.loc[temp.Position.isin(list(range(int(0.2*total_ranks+1),int(0.4*total_ranks+1))))].assign(Rank=2)
                temp.loc[temp.Position.isin(list(range(int(0.2*total_ranks+1),int(0.4*total_ranks+1))))] = temp.loc[temp.Position.isin(list(range(int(0.2*total_ranks+1),int(0.4*total_ranks+1))))].assign(Rank2=1.0)

            elif(temp.loc[temp.Position.isin(list(range(int(0.4*total_ranks+1),int(0.6*total_ranks+1))))].empty==False):
                temp.loc[temp.Position.isin(list(range(int(0.4*total_ranks+1),int(0.6*total_ranks+1))))] = temp.loc[temp.Position.isin(list(range(int(0.4*total_ranks+1),int(0.6*total_ranks+1))))].assign(Rank=3)
                temp.loc[temp.Position.isin(list(range(int(0.4*total_ranks+1),int(0.6*total_ranks+1))))] = temp.loc[temp.Position.isin(list(range(int(0.4*total_ranks+1),int(0.6*total_ranks+1))))].assign(Rank3=1.0)

            elif(temp.loc[temp.Position.isin(list(range(int(0.6*total_ranks+1),int(0.8*total_ranks+1))))].empty==False):
                temp.loc[temp.Position.isin(list(range(int(0.6*total_ranks+1),int(0.8*total_ranks+1))))] = temp.loc[temp.Position.isin(list(range(int(0.6*total_ranks+1),int(0.8*total_ranks+1))))].assign(Rank=4)
                temp.loc[temp.Position.isin(list(range(int(0.6*total_ranks+1),int(0.8*total_ranks+1))))] = temp.loc[temp.Position.isin(list(range(int(0.6*total_ranks+1),int(0.8*total_ranks+1))))].assign(Rank4=1.0)

            elif(temp.loc[temp.Position.isin(list(range(int(0.8*total_ranks+1),int(total_ranks+1))))].empty==False):
                temp.loc[temp.Position.isin(list(range(int(0.8*total_ranks+1),int(total_ranks+1))))] = temp.loc[temp.Position.isin(list(range(int(0.8*total_ranks+1),int(total_ranks+1))))].assign(Rank=5)
                temp.loc[temp.Position.isin(list(range(int(0.8*total_ranks+1),int(total_ranks+1))))] = temp.loc[temp.Position.isin(list(range(int(0.8*total_ranks+1),int(total_ranks+1))))].assign(Rank5=1.0)
            temp = temp.fillna(0)
            Series_per_position.iloc[i,2:Series_per_position.shape[1]] = temp.mean(axis = 0).iloc[1:temp.shape[1]]

        Series_per_position = Series_per_position.drop('Series', axis = 1)
        ranking = pd.merge(ranking,Series_per_position, on = "Position")
        ranking = ranking[["ID", "Return", "Position", "Rank", "Rank1", "Rank2", "Rank3", "Rank4", "Rank5"]]
        ranking = ranking.sort_values(["Position"])

    rps_sub = (
        (submission.set_index('ID').filter(regex=r'Rank\d').cumsum(axis=1)
        - ranking.set_index('ID').filter(regex=r'Rank\d').cumsum(axis=1))**2
    ).mean(axis=1).mean()

    output = {'RPS' : rps_sub,
              'details' : submission}

    return(output)

#Function for computing IR
def IR_calculation(hist_data, submission):

    asset_id = pd.unique(hist_data.symbol)
    asset_id = sorted(asset_id)

    #Compute percentage returns
    returns = pd.DataFrame(columns = ["ID", "Return"])

    #Investment weights
    weights = submission[["ID","Decision"]]

    pivoted = hist_data.pivot_table(index=['date'], columns=['symbol'], values=['price']).pct_change(axis=0).iloc[1:]
    pivoted.columns = pivoted.columns.get_level_values(1)
    ret = np.log(1+(pivoted.reindex(columns=asset_id) * weights.set_index('ID').reindex(asset_id)['Decision'].to_numpy()).sum(axis=1)).reset_index(drop=True)
    sum_ret = sum(ret)
    sdp = stdev(ret)
    
    output = {'IR' : sum_ret/sdp,
              'details' : list(ret)}
    return output

#Run evaluation
print('RPS: ', RPS_calculation(hist_data = hist_data , submission = submission_data)['RPS'])

print('IR: ', IR_calculation(hist_data, submission)['IR'])

