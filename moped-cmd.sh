#!/usr/bin/env bash
#
# Sends one command to a running Moped instance's local debug/automation socket and prints its
# response (if any). See CLAUDE.md and src/main/scala/impl/Server.scala for the full protocol.
#
# Usage:
#   ./moped-cmd.sh point
#   ./moped-cmd.sh mark
#   ./moped-cmd.sh line 0
#   ./moped-cmd.sh invoke forward-char
#   ./moped-cmd.sh type "hello world"     # types into an active minibuffer prompt if one is
#                                          # showing (e.g. after `invoke rename-element`), else
#                                          # into the focused buffer
#   ./moped-cmd.sh key y               # simulates pressing 'y', for prompts (like mini-yesno)
#                                       # whose keys don't just self-insert into a text buffer
#   ./moped-cmd.sh click 120 45
#   ./moped-cmd.sh click 120 45 2      # double-click (word select); 3 for triple (line select)
#   ./moped-cmd.sh drag 200 45
#   ./moped-cmd.sh release 200 45
#   ./moped-cmd.sh screenshot /tmp/shot.png
#   ./moped-cmd.sh screenshot /tmp/shot.png 0 0 400 200
#
# Set MOPED_PORT to match the running instance if it's not using the default port (32324).
#
# Note: `open PATH` has no response, so this will sit for the full timeout before returning
# nothing; that command is normally sent by Moped's own launcher, not this script.

set -euo pipefail

port="${MOPED_PORT:-32324}"

if [ "$#" -eq 0 ]; then
  echo "Usage: $0 <command> [args...]" >&2
  exit 1
fi

exec 3<>"/dev/tcp/localhost/$port"
echo "$*" >&3
if read -r -t 6 response <&3; then
  echo "$response"
fi
exec 3<&-
