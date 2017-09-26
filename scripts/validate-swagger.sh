#!/bin/bash

# Validate Swagger file using online validator

# Swagger 2.0 does not support anyOf (among others)
# https://github.com/OAI/OpenAPI-Specification/issues/57
# Swagger 3.0 does but no validator like before afaict
exit 0

URL='http://online.swagger.io/validator/debug'
SWAGGER=../applications/crossbar/priv/api/swagger.json

tmp=$RANDOM.json
curl -o $tmp "$URL" -X POST -d @$SWAGGER -H 'Content-Type:application/json' || exit 2
"$(dirname "$0")"/format-json.sh $tmp || (rm $tmp && exit 3)
errors=$(cat $tmp | python2 -c 'import sys, json; print len(json.load(sys.stdin).get("schemaValidationMessages", []))')
[[ $errors -ne 0 ]] && echo Swagger file validation errors: $errors && cat $tmp
rm $tmp

exit $errors
