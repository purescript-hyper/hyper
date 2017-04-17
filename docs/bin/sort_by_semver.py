#!/usr/bin/env python

import fileinput
import semver


def sort_versions(versions): return sorted(versions, cmp=semver.compare,
                                           reverse=True)


def main():
    versions = map(lambda line: line.strip(), list(fileinput.input()))
    sorted_versions = sort_versions(versions)
    for version in sorted_versions:
        print(version)


if __name__ == '__main__':
    main()
