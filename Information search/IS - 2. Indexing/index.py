    # -*- coding: utf-8 -*-

import docreader
import doc2words
import pickle
import mmh3
import json
import sys

import varbyte

args = docreader.parse_command_line ().files
encoder_arg  = args [0]
archive_args = args [1:]

if encoder_arg == 'varbyte':
    encoder = varbyte
else:
    print "Unsupported encoder"
    exit ()

dictionary = {}
urls       = []

""" Reading dataset file """
counter = 0
for entry in docreader.DocumentStreamReader (archive_args):
    urls.append (entry.url)
    counter += 1

    for word in set (doc2words.extract_words (entry.text)):
        hash = abs (mmh3.hash (word.encode ("utf-8").lower ()))
        if not dictionary.get (hash):
            dictionary [hash] = []

        dictionary [hash].append (counter)

""" Compressing dictionary """
dictionary = { 
    entry: [
        encoder.encode (id) for id in dictionary [entry]
    ] for entry in dictionary 
}

""" Storing index in memory """
desc = open ("./index_encoder", "w")
desc.write (encoder_arg)
desc.close ()

desc = open ("./index_dictionary", "w")
pickle.dump (dictionary, desc)
desc.close  ()

desc = open ("./index_urls", "w")
pickle.dump (urls, desc)
desc.close  ()