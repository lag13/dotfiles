#!/usr/bin/env bash

# https://gist.github.com/avoidik/c2f77e8f1afa38d20723a46a88b833d2

function walk() {
  for secret in $(vault list $1 | tail -n +3)
  do
    if [[ ${secret} == *"/" ]] ; then
      walk "${1}${secret}"
    else
      echo "${1}${secret}"
    fi
  done
}

query="${1}"

if [[ ${query} != *"/" ]] ; then
  query=${query}/
fi

echo "${1}"
walk ${query}
