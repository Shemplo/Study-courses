# coding: utf-8

import random
import sys
import re

import urlparse
import urllib

samples_size = 1000
treshold = 0.1

def read_file_and_get_subset (file, limit):
    lines = []
    desc = open (file, "r")
    for line in desc:
        lines.append (line [:-1])
    desc.close ()

    sample = range (len (lines))
    random.shuffle (sample)

    return (lines, sample [:limit])

def increment_counter (key, counters):
    if counters.get (key):
        counters [key] += 1
    else:
        counters [key] = 1

def handle_url (path, query, counters):
    templates = {
        "a": ["segment_name_",            r".*",               lambda s, m: s],
        "b": ["segment_[0-9]_",           r"[0-9]+$|[0-9]+\.", lambda s, m: "1"],
        "c": ["segment_substr[0-9]_",     r"[^\d]+\d+[^\d]+$", lambda s, m: "1"],
        "d": ["segment_ext_",             r".+\.(.+)",         lambda s, m: m.group (1)],
        "e": ["segment_ext_substr[0-9]_", r"[^\d]+\d+[^\d]+\.(\W+)", 
         lambda s, m: m.group (1)
        ],
        "f": ["segment_len_",             r".*",               lambda s, m: str (len (s))]
    }

    index = 0
    for seg in urllib.unquote (path).split ('/'):
        if seg == "":
            continue

        for k in templates:
            temp = templates [k]
            match = re.match (temp [1], seg)
            if match:
                increment_counter (temp [0] + str (index) + ":" + temp [2] (seg, match), counters)

        index += 1

    increment_counter ("segments:" + str (index), counters)

    for seg in urllib.unquote (query).split ('&'):
        if seg == "":
            continue

        match = re.match (r"(.+)=(.+)", seg)
        if match:
            increment_counter ("param_name:" + match.group (1), counters)
            increment_counter ("param:" + match.group (), counters)
        else:
            increment_counter ("param_name:" + seg, counters)

def write_results_file (file, counters):
    desc = open (file, "w")

    for k in counters.keys ():
        if counters [k] > treshold * samples_size:
            desc.write (k + '\t' + str (counters [k]) + '\n')

    desc.close ()

def extract_features (INPUT_FILE_1, INPUT_FILE_2, OUTPUT_FILE):
    counters = {}

    for f in [INPUT_FILE_1, INPUT_FILE_2]:
        subset = read_file_and_get_subset (f, samples_size)
        for i in subset [1]:
            url_parsed = urlparse.urlparse (subset [0][i])
            handle_url (url_parsed.path, url_parsed.query, counters)

    write_results_file (OUTPUT_FILE, counters)
