#
# Regular cron jobs for the snow2-client package
#
0 4	* * *	root	[ -x /usr/bin/snow2-client_maintenance ] && /usr/bin/snow2-client_maintenance
