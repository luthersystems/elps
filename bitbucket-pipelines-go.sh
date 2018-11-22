# A general-purpose setup script for Go (golang) projects using Bitbucket Pipelines.
#
# This script sets the IMPORT_PATH environment variable, so it should be sourced in your
# bitbucket-pipelines.yml file. 
#
# Example:
#     Source this script
#
#         - source bitbucket-pipelines-go.sh
#
#     Use IMPORT_PATH
#
#         - cd ${IMPORT_PATH}
#         - go build
#
# References:
#     * https://golang.org/doc/code.html#ImportPaths
#     * https://confluence.atlassian.com/display/BITBUCKET/Environment+variables+in+Bitbucket+Pipelines
#     * https://hub.docker.com/_/golang/

BASE_PATH="${GOPATH}/src/bitbucket.org/${BITBUCKET_REPO_OWNER}"
mkdir -p ${BASE_PATH}
export IMPORT_PATH="${BASE_PATH}/${BITBUCKET_REPO_SLUG}"
ln -s ${PWD} ${IMPORT_PATH}
