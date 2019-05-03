# coding: utf-8

from __future__ import division

import base64
import csv
import gzip
import zlib
from collections import namedtuple

import numpy as np
from itertools import islice, imap

from sklearn.feature_extraction.text import CountVectorizer,TfidfTransformer
from sklearn.linear_model import SGDClassifier
from sklearn.pipeline import Pipeline
from sklearn import metrics

from stop_words import get_stop_words

from bs4 import BeautifulSoup

# Traces
TRACE_NUM = 1000
import logging
reload(logging)
logging.basicConfig(format='%(asctime)s %(levelname)s:%(message)s', level=logging.INFO, datefmt='%H:%M:%S')

def trace(items_num, trace_num=TRACE_NUM):
    if items_num % trace_num == 0: logging.info("Complete items %05d" % items_num)

# Data loading
DocItem = namedtuple('DocItem', ['doc_id', 'is_spam', 'url', 'html_data'])

def load_csv(input_file_name):    
    with gzip.open(input_file_name) if input_file_name.endswith('gz') else open(input_file_name)  as input_file:            
        headers = input_file.readline()
        
        for i, line in enumerate(input_file):
            trace(i)
            parts = line.strip().split('\t')
            url_id = int(parts[0])                                        
            mark = bool(int(parts[1]))                    
            url = parts[2]
            pageInb64 = parts[3]
            html_data = base64.b64decode(pageInb64)
            yield DocItem(url_id, mark, url, html_data)
                
        trace(i, 1)

# Fetching sequences
def get_row (data, function):
    return list (map (function , data)) 

# Stopwords
rus_stopwords = get_stop_words ('russian')
eng_stopwords = get_stop_words ('english')
stopwords = rus_stopwords + eng_stopwords

# Text parsing
def convert2lower(f):
    def tmp(text):        
        return f(text.lower())
    return tmp

@convert2lower
def easy_tokenizer(text):
    word = ''
    for symbol in text:
        if symbol.isalnum(): word += symbol
        elif word:
            yield word
            word = ''
    if word: yield word

def tokenize (text):
    return list (easy_tokenizer (text))

# HTML parsing
def make_html_soup (raw_html):
    return BeautifulSoup (raw_html, "html.parser")

def fetch_spec_tag (soup, tag_name):
    return soup.find_all (tag_name)

def number_of_spec_tag (soup, tag_name):
    return len (fetch_spec_tag (soup, tag_name))

def has_spec_tag (soup, tag_name):
    return int (number_of_spec_tag (soup, tag_name) > 0)

def size_of_tag_content (soup, tag_name):
    tag = fetch_spec_tag (soup, tag_name)
    if not tag:
        return 0
    else:
        return len (tag [0].get_text ())

# Feature extraction
def extract_features (raw_html):
    soup = make_html_soup (raw_html)
    words = tokenize (raw_html)
    fts = []

    fts.append (len (raw_html))               # length of document
    fts.append (len (words))                  # number of words
    fts.append (len (raw_html) / len (words)) # agerage word length

    return fts

cntvec_t = CountVectorizer (stop_words = stopwords, ngram_range = (1, 2))
tfidf_t = TfidfTransformer ()

def process_tfidf (raw_htmls):
    cntvec = cntvec_t.fit_transform (raw_htmls)
    return tfidf_t.fit_transform (cntvec)

# Train data
TRAIN_DATA_FILE  = 'kaggle_train_data_tab.csv.gz'
train_data = list (load_csv (TRAIN_DATA_FILE))

xs = get_row (train_data, lambda x : x.html_data)
xsf1 = map (extract_features, xs)
print ("fetures")
#xsf2 = process_tfidf (xs)
#print ("tf-idf")

#xsf = [xsf1 [i] + xsf2 [i] for i in range (len (xs))]

clf = SGDClassifier (n_jobs = 3)
clf.fit (xsf1, get_row (train_data, lambda x : x.is_spam))

# Test data
TEST_DATA_FILE  = 'kaggle_test_data_tab.csv.gz'
test_docs = load_csv(TEST_DATA_FILE)

with open('my_submission.csv' , 'wb') as fout:
    writer = csv.writer (fout)
    writer.writerow (['Id','Prediction'])
    for item in test_docs:
        #cntvec = cntvec_t.fit_transform ([item.html_data])
        #tfidf = tfidf_t.fit_transform (cntvec)
        fts = extract_features (item.html_data)
        prediction = 1 if clf.predict ([fts]) [0] else 0
        writer.writerow ([item.doc_id, prediction])