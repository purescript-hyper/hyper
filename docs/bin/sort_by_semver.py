#!/usr/bin/env python

import fileinput
import semver


def cmp_semver(a, b):
    try:
        return semver.compare(a, b)
    except:
        return cmp(a, b)


def sort_versions(versions): return sorted(versions, cmp=cmp_semver,
                                           reverse=True)


def main():
    versions = map(lambda line: line.strip(), list(fileinput.input()))
    sorted_versions = sort_versions(versions)
    for version in sorted_versions:
        print(version)


if __name__ == '__main__':
    main()
