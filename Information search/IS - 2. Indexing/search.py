# -*- coding: utf-8 -*-

import cPickle as pickle
import varbyte
import mmh3
import sys
import re

def search (query, dictionary, encoder):
    first = True
    result = []

    for token in re.split (r"\s+\&\s+", query):
        token = token.encode ("utf-8")
        hash = abs (mmh3.hash (token))
        if not dictionary.get (hash):
            return []

        docs = [encoder.decode (id) for id in dictionary [hash]]
        if first:
            result = docs
            first = False
        else:
            result = list (set (result) & set (docs))

    return sorted (result)

""" Reading stored data """
desc = open ("./index_encoder", "r")
encoder_arg = desc.readline ()
desc.close ()

if encoder_arg == 'varbyte':
    encoder = varbyte
else:
    print "Unsupported encoder"
    exit ()

desc = open ("./index_dictionary", "r")
dictionary = pickle.load (desc)
desc.close ()

desc = open ("./index_urls", "r")
urls = pickle.load (desc)
desc.close ()

while True:
    try:
        query = raw_input ().decode ("utf-8")
        docs = search (query.lower (), dictionary, encoder)
        print query.encode ("utf-8")
        print len (docs)
        for doc in docs:
            print urls [doc - 1]
    except:
        break