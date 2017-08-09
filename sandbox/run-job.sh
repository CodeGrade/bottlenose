#!/bin/bash
NAME=$1
DRVR=$2

echo "## Pushing $DRVR..."
lxc file push "$DRVR" "$NAME/root/driver.pl" || echo "Error: $?"

echo "## Running driver..."
lxc exec "$NAME" -- bash -c "perl /root/driver.pl" || echo "Error: $?"

# FIXME: Sandbox may not live long enough for
#    monitor page to load. Maybe should preserve raw output data after run.
sleep 5
