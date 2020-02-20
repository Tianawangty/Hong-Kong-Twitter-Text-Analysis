#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np
import re
from re import sub
import multiprocessing
import statistics


from gensim.models.phrases import Phrases, Phraser
from gensim.models import Word2Vec
from gensim.test.utils import get_tmpfile
from gensim.models import KeyedVectors

from sklearn.cluster import KMeans
from time import time 
from collections import defaultdict

import logging  # Setting up the loggings to monitor gensim
logging.basicConfig(format="%(levelname)s - %(asctime)s: %(message)s", datefmt= '%H:%M:%S', level=logging.INFO)


# In[2]:


df = pd.read_csv('jun_anti.csv')
df.head()


# In[3]:


text2 = df[['text','id']]
text2


# In[4]:


text2_list = []
for i in range(len(text2)):
    text2_list.append(re.sub('[@#]', '', str(text2['text'][i])))

text2_list


# In[5]:


text2['text'] = text2_list


# In[6]:


text2['text'] = text2['text'].str.replace('http\S+|www.\S+', '', case=False)


# In[7]:


text2['text']


# In[8]:


text2['text'] = df[['text']]


# In[9]:


df.head()


# In[10]:


text2['text'] = text2['text'].astype(str)


# In[11]:


list_of_word=[]


# In[12]:


for i in range(len(text2['text'])):
    list_of_word.append(text2['text'][i].split())
text2['words']=list_of_word
text2


# In[13]:


sent = [x for x in text2.words]
phrases = Phrases(sent, min_count=1, progress_per=50000)
bigram = Phraser(phrases)
sentences = bigram[sent]
sentences[1]


# ## VADER

# In[14]:


#pip install --upgrade vaderSentiment


# In[15]:


from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
analyser = SentimentIntensityAnalyzer()


# In[16]:


def sentiment_analyzer_scores(sentence):
    score = analyser.polarity_scores(sentence)
    print("{:-<40} {}".format(sentence, str(score)))


# In[17]:


i=0 #counter

compval1 = [ ]  #empty list to hold our computed 'compound' VADER scores


while (i<len(text2)):

    k = analyser.polarity_scores(text2.iloc[i]['text'])
    compval1.append(k['compound'])
    
    i = i+1
    
#converting sentiment values to numpy for easier usage

compval1 = np.array(compval1)

len(compval1)


# In[20]:


text2['sentiment'] = compval1


# In[22]:


i = 0

predicted_value = [ ] #empty series to hold our predicted values

while(i<len(text2)):
    if ((text2.iloc[i]['sentiment'] >= 0.05)):
        predicted_value.append('Positive')
        i = i+1
    elif ((text2.iloc[i]['sentiment'] > -0.05) & (text2.iloc[i]['sentiment'] < 0.05)):
        predicted_value.append('Neutral')
        i = i+1
    elif ((text2.iloc[i]['sentiment'] <= -0.05)):
        predicted_value.append('Negative')
        i = i+1


# In[23]:


text2['sentiment_class'] = predicted_value


# In[24]:


text2['id'] = df['id']
text2['time'] = df['created_at']
text2


# In[25]:


final_file = df.merge(text2, left_on='id', right_on='id')
final_file = final_file.drop(['text_y','words','time'],axis=1)
final_file


# In[26]:


final_file.to_csv('jun_anti_senti.csv')


# The compound score is computed by summing the valence scores of each word in the lexicon, adjusted according to the rules, and then normalized to be between -1 (most extreme negative) and +1 (most extreme positive). 
