#!/usr/bin/env python

from functools import cmp_to_key
import fileinput
import semver


def cmp_semver(a, b):
    try:
        return semver.compare(a, b)
    except:
        return cmp(a, b)


def sort_versions(versions): return sorted(versions, key=cmp_to_key(cmp_semver),
                                           reverse=True)


def main():
    versions = map(lambda line: line.strip()[1:], list(fileinput.input()))
    sorted_versions = sort_versions(versions)
    for version in sorted_versions:
        print(version)


if __name__ == '__main__':
    main()
