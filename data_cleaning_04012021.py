import pandas as pd
import numpy as np
import openpyxl
import xlrd as xlrd
import datetime as datetime
from datetime import datetime
import researchpy as rp
import matplotlib.pyplot as plt
who_stats_2020, \
world_dev_indicators, who_covid = 'EN_WHS_2020_Annex2', \
                                       'Data_Extract_From_World_Development_Indicators', \
                                       'WHO COVID-19 global table data April 1st 2021 at 2.56.27 PM'
data_from_file = '~/Downloads/{file_named}.xlsx'.format(file_named=who_stats_2020)

data_from_worldfile = '~/Downloads/{file_named}.xlsx'.format(file_named=world_dev_indicators)
f = pd.read_excel(data_from_file, sheet_name=[1])
t = pd.read_excel(data_from_worldfile, sheet_name=[0])
df = f[1]
df4= t[0]

data_from__csv_file = '~/Downloads/{file_named}.csv'.format(file_named=who_covid)
f1 = pd.read_csv(data_from__csv_file)
f1 = f1.set_index('Name')
df4 = df4.set_index('Country Name')
df1 = df[['Unnamed: 0', 'Unnamed: 3']][4:].set_index('Unnamed: 0')

df2 = df1.join(f1, how='outer')

df2=df2.rename(columns={'Unnamed: 3': 'total_pop'}).dropna()
df2['total_pop'] = df2['total_pop']*1000
df3 = df2.join(df4, how='left')


def column_cleaning(df):
    df.columns = df.columns.str.lower()
    df.columns = df.columns.str.strip()
    df.columns = df.columns.str.replace(' ', '_')
    df.columns = df.columns.str.replace('?', '')
    df=df.replace('..', np.NaN)
column_cleaning(df3)

def cleaning(df):
    df=df.dropna(subset=['total_pop']).dropna(axis='columns', how='all')[(df.time_code == 'YR2019')]
    africa = df.loc[(df.who_region == 'Africa')]
    europe = df.loc[(df.who_region == 'Europe')]
    americas = df.loc[(df.who_region == 'Americas')]
    middle_e = df.loc[(df.who_region == 'Eastern Mediterranean')]
    continents = [africa, europe, americas, middle_e]
    for i, h in enumerate(continents):
        continents[i] = continents[i].dropna(axis='columns', how = 'any').select_dtypes(exclude=['object'])
        group_names = ['Low', 'Medium', 'High']
        bins_death = np.linspace(min(continents[i]['deaths_-_cumulative_total_per_100000_population']),
                                 max(continents[i]['deaths_-_cumulative_total_per_100000_population']), 4)
        continents[i]['death_rate_cat'] = pd.cut(continents[i]['deaths_-_cumulative_total_per_100000_population'],
                                                 bins_death, labels=group_names, include_lowest=True).cat.codes
        corr_matrix = continents[i].corr()['death_rate_cat'].sort_values(ascending=False).head(25)
        corr_matrix_list = corr_matrix.index.to_list()
        continents[i] = continents[i].filter(corr_matrix_list, axis=1)
    return continents
top_25_vars = cleaning(df3)

def strat_split_transform(df):
    from sklearn.pipeline import Pipeline
    from sklearn.preprocessing import StandardScaler
    from sklearn.model_selection import StratifiedShuffleSplit
    split = StratifiedShuffleSplit(n_splits=1, test_size=0.15, random_state=42)
    # for train_index, test_index in split.split(df, )

def split_transform(df):
    from sklearn.model_selection import train_test_split
    from sklearn import preprocessing
    y = df[['death_rate_cat']]
    features = df.drop(['death_rate_cat'], axis=1)
    X = preprocessing.StandardScaler().fit(features).transform(features.astype(float))
    X_trainset, X_testset, y_trainset, y_testset = train_test_split(X, y, test_size=0.3, random_state=3)

    import statsmodels.formula.api as sm
    df.columns = df.columns.str[:10]
    df.columns = df.columns.str.replace('_-_', '')
    formula_str = df.columns[0]+' ~ '+'+'.join(df.columns[1:])
    model = sm.ols(formula=formula_str, data=df)
    fitted = model.fit()
    fitted_summary = fitted.summary()
    print('OLS Regression:\n'+str(fitted.summary()))
    ols_results = pd.DataFrame()
    ols_results['p_value'] = fitted.pvalues[1:]
    ols_results['features'] = df.columns[1:]
    ols_results.set_index('features', inplace=True)
    def yes_no(b):
        if b < 0.05:
            return 'Yes'
        else:
            return 'No'
    ols_results['statistically_significant'] = ols_results['p_value'].apply(yes_no)
    return X_trainset, X_testset, y_trainset, y_testset, fitted_summary, ols_results

africa = top_25_vars[0]
test = split_transform(africa)


def dummy_classifer(X_trainset, X_testset, y_trainset, y_testset):
    '''determines baseline performance,
    success rate of the model if simply guessing
    '''
    from sklearn.dummy import DummyClassifier
    clf = DummyClassifier(strategy='most_frequent').fit(X_trainset, y_trainset)
    y_pred = clf.predict(X_testset)
    print('y actual: \n' + str(y_testset.value_counts()))
    print('y actual: \n' + str(pd.Series(y_pred).value_counts()))
    # Model Evaluation metrics
    from sklearn.metrics import accuracy_score, recall_score, precision_score, f1_score
    print('Accuracy Score : ' + str(accuracy_score(y_testset, y_pred)))
    print('Precision Score : ' + str(precision_score(y_testset, y_pred)))
    print('Recall Score : ' + str(recall_score(y_testset, y_pred)))
    print('F1 Score : ' + str(f1_score(y_testset, y_pred)))

    # Dummy Classifier Confusion matrix
    from sklearn.metrics import confusion_matrix
    print('Confusion Matrix : \n' + str(confusion_matrix(y_testset, y_pred)))
d_class=dummy_classifer(test[0], test[1], test[2], test[3])
def grid_search(X_trainset, X_testset, y_trainset, y_testset):
    from sklearn.model_selection import GridSearchCV
    from sklearn.linear_model import LogisticRegression
    from sklearn.metrics import confusion_matrix
    from sklearn.metrics import accuracy_score, recall_score, precision_score, f1_score
    clf = LogisticRegression()
    grid_values = {'penalty': ['l1', 'l2'], 'C': [0.001, .009, 0.01, .09, 1, 5, 10, 25]}
    grid_clf_acc = GridSearchCV(clf, param_grid=grid_values, scoring='recall')
    grid_clf_acc.fit(X_trainset, y_trainset)

    # Predict values based on new parameters
    y_pred_acc = grid_clf_acc.predict(X_testset)

    # New Model Evaluation metrics
    print('Accuracy Score : ' + str(accuracy_score(y_testset, y_pred_acc)))
    print('Precision Score : ' + str(precision_score(y_testset, y_pred_acc)))
    print('Recall Score : ' + str(recall_score(y_testset, y_pred_acc)))
    print('F1 Score : ' + str(f1_score(y_testset, y_pred_acc)))

    # Logistic Regression (Grid Search) Confusion matrix
    confusion_matrix(y_testset, y_pred_acc)

d_grid=grid_search(test[0], test[1], test[2], test[3])

