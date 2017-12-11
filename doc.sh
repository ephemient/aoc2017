#!/bin/bash
. "$(git --exec-path)/git-sh-setup" || exit $?
set -euxo pipefail
stack haddock aoc2017:lib
export GIT_INDEX_FILE=$(mktemp -t aoc2017-doc.XXXXXX)
trap 'rm -f "${GIT_INDEX_FILE}"' EXIT
git read-tree --empty
find .stack-work/dist/*/*/doc/html/aoc2017 -type f -printf '%p\0%P\0' |
while { read -r -d '' file; read -r -d '' path; }; do
    [[ -x ${file} ]] && mode=100755 || mode=100644
    sha1=$(git hash-object -t blob -w --path "${path}" -- "${file}")
    printf '%s %s\t%s\0' "${mode}" "${sha1}" "${path}"
done | git update-index -z --index-info
tree=$(git write-tree)
[[ ${tree} == $(git rev-parse refs/heads/gh-pages^{tree}) ]] ||
git update-ref refs/heads/gh-pages "$(git commit-tree "${tree}" \
    -p refs/heads/gh-pages -m "stack haddock @ $(git rev-parse --short HEAD)")"
