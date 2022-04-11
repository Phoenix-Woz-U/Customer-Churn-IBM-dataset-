#!/usr/bin/env python
# coding: utf-8

# # PHOENIX Final Project

# # Import packages

# In[2]:


import pandas as pd
import numpy as np 
from scipy.stats import norm
from scipy import stats
from scipy.stats import ttest_ind
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm


# # Load Data

# In[3]:


Telcochurn = pd.read_excel("C:/Users/Dell 1/OneDrive/Documentos/Data Science Final Project/Telco_customer_churn.xlsx")


# In[4]:


Telcochurn.head()


# # Finding independent variable

# In[5]:


Telcochurn.describe()


# In[6]:


def report(df):
    col = []
    d_type = []
    uniques = []
    n_uniques = []
    
    for i in df.columns:
        col.append(i)
        d_type.append(df[i].dtypes)
        uniques.append(df[i].unique()[:5])
        n_uniques.append(df[i].nunique())
    
    return pd.DataFrame({'Column': col, 'd_type': d_type, 'unique_sample': uniques, 'n_uniques': n_uniques})


# In[9]:


report(Telcochurn)


# ## Our first intention is to resume the data to have a complete picture of it and then be able to select only columns that can help to determine the probability of customers churning.

# In[7]:


Telcochurn.columns


# In[13]:


Telcochurn1 = Telcochurn


# In[14]:


Telcochurn1.info


# In[15]:


Telcochurn1.drop(columns=["CustomerID", "Count", "Lat Long", "Latitude", "Longitude", "Country", "State", "Partner", "Dependents", "Streaming TV", "Streaming Movies", "Paperless Billing"], axis=1, inplace=True)


# In[16]:


Telcochurn1.head()


# In[14]:


Telcochurn1.info


# # How it looks?

# In[17]:


Telcochurn1.hist(bins=21,figsize=(15,15))


# In[18]:


stats.ttest_1samp(Telcochurn1['Churn Score'], 6.2)


# In[19]:


Telcochurn1['Churn Reason'].value_counts()


# In[20]:


Telcochurn1['Churn Reason'].value_counts().plot(kind='bar')


# In[21]:


Telcochurn1['Senior Citizen'].value_counts()


# In[22]:


sns.catplot(x='Gender', col='Senior Citizen', hue='Churn Label', kind='count', data=Telcochurn1)


# In[23]:


by_gender_senior = Telcochurn1.groupby(['Senior Citizen', 'Gender'])['Churn Label'].value_counts(normalize=True).to_frame().rename(columns={'Churn Label': 'Ratio'}).reset_index().sort_values('Senior Citizen')
by_gender_senior


# ## The visualization above shows that the churn and retain ratio for both male and female senior citizens is almost the same.  We can also appreciate an increase in the churn proportion in this same category of customers.  According to the analysis of nonsenior citizens, of both genders, the churn proportion keeps the balance very similar.
# 

# #  Comparing Senior Citizens  retention by Gender according to contract lenght.

# In[24]:


Telcochurn2 = Telcochurn1


# In[18]:


Telcochurn2.info()


# In[25]:


Telcochurn2.head()


# In[26]:


by_gender_senior = Telcochurn2.groupby(['Senior Citizen', 'Gender'])['Contract'].value_counts(normalize=True).to_frame().rename(columns={'Contract': 'Contract Length'}).reset_index().sort_values('Senior Citizen')
by_gender_senior


# In[27]:


Plot = Telcochurn2.plot.scatter(x='Gender', y='Contract')
Plot.set_title("Gender / Contract Lenght")
Plot.set_xlabel("Gender")
Plot.set_ylabel("Contract")


# In[28]:


plt.figure(figsize=(10,7),facecolor='white')
sns.heatmap(pd.crosstab(Telcochurn2.Gender,Telcochurn2.Contract),annot=True,fmt="d",cmap="Blues")
plt.title('Gender & Contract\n ',fontsize=28,color='Black')


# ## Spending average

# In[29]:


Telcochurn2['Total Charges']=Telcochurn2['Total Charges'].apply


# In[30]:


print(Telcochurn2.isnull().sum())
print('The sum of values is null: ', Telcochurn2.isnull().sum().sum())


# In[31]:


Telcochurn2.corr(method='pearson').style.format("{:.2}").background_gradient(cmap=plt.get_cmap('coolwarm'), axis=1)


# In[ ]:


Telcochurn2['Total Charges'].value_counts().plot(kind='bar')


# In[ ]:




