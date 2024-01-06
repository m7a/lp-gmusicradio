#!/bin/sh -eu
# Script to enqueue news podcast episodes when they appear and when they are
# “new” i.e. newsworthy, (c) 2023-11-25 Ma_Sys.ma <info@masysma.net>

scriptroot="$(cd "$(dirname "$0")" && pwd)"
list="$scriptroot/state/list.txt"
newl="$scriptroot/state/new.txt"

if [ $# = 0 ]; then
	podget -d "$scriptroot/conf"

	find "$scriptroot/pod/deutschlandfunk" -type f | sort > "$newl"
	if [ -f "$list" ]; then
		if diff -u "$list" "$newl" | grep -qE "^\+"; then
			# entries added, this news is worth reporting
			gmusicbrowser -enqueue "$(tail -n 1 "$newl")"
			mv "$list" "$newl"
		else
			# nothing changed
			rm "$newl"
		fi
	else
		# first time do not play news
		mv "$newl" "$list"
	fi
else
	case "$1" in
	(--help|-h)
		echo "USAGE $0 [--start|--loop]"
		;;
	(--start)
		if [ -f "$list" ]; then
			rm "$list"
		fi
		exec "$0" --loop
		;;
	(--loop)
		while true; do
			"$0"
			sleep 600
		done
		exit 1
		;;
	(*)
		echo "Unknown option."
		exit 1;;
	esac
fi
