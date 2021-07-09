#!/bin/sh
API_URL="https://poses.live"
if [ -z "$API_TOKEN" ]; then
	echo "export API_TOKEN=..." >2
	exit 1
fi
AUTH_HEADER="Authorization: Bearer $API_TOKEN"

case $1 in
	ping)
		curl -s -H "$AUTH_HEADER" "$API_URL/api/hello"
		;;
	download)
		curl -s -H "$AUTH_HEADER" "$API_URL/api/problems/$2"
		;;
	solve)
		curl -s -H "$AUTH_HEADER" -X POST "$API_URL/api/problems/$2/solutions" --data-binary @-
		;;
	*)
		echo "unknown command: $1" >2
		exit 1
		;;
esac
