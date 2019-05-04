#! /usr/bin/env python
# -*- coding: utf-8 -*

import sys
import argparse


def lev_dist(src, dst):
    n = len(src)
    m = len(dst)
    dp = {}

    for i in xrange(n + 1):
        dp[i, 0] = i

    for j in xrange(m + 1):
        dp[0, j] = j

    for i in xrange(1, n + 1):
        for j in xrange(1, m + 1):
            dp[i, j] = min(dp[i - 1, j] + 1, dp[i, j - 1] + 1,
                           dp[i - 1, j - 1] + (src[i - 1] != dst[j - 1]))

    return dp[(n, m)]


def load_true_labels(filename):
    labels = {}
    with open(filename) as f:
        for line in f:
            original, expected, _ = line.strip().decode('utf8').split(',', 2)
            assert(original not in labels)
            labels[original] = expected
    return labels


def parse_args():
    parser = argparse.ArgumentParser(description='Evaluate submission file')
    parser.add_argument('-l', '--labels', type=str, required=True,
                        help='tsv file with ground true labels')
    return parser.parse_args()


def main():
    args = parse_args()

    true_labels = load_true_labels(args.labels)

    sum_val = 0
    count_val = 0

    for line in sys.stdin:
        original, fix = line.decode('utf8').strip('\n').split(',')
        if original not in true_labels:
            # unknown id, just skip it
            continue
        expected = true_labels[original]
        sum_val += lev_dist(fix, expected)
        count_val += 1

    print 1.0 * sum_val / count_val


if __name__ == '__main__':
    main()
