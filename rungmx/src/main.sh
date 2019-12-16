#!/usr/bin/env bash

# GROMACS setting from ENV
GMX=${GMX:="gmx"}
GMX_FLAGS=${GMX_FLAGS:="-v -quiet"}

# Default settings
TOPOL="*.top"
CONF="*.gro"
MDP_PATH="mdp/"
MDP=()
LOGFILE=${LOGFILE:="rungmx.log"}
SCT=0

# -------------------------------

# Write to stderr
error () {
  >&2 echo -e '\033[1;91m'ERROR:'\033[0m' $@
}

# Simultanious write to screen and logfile.
write () {
  local GREEN='\033[0;32m'
  local NOR='\033[0m'
  echo -e "${GREEN}$@${NOR}" >&2
  echo -e "$@" >> $LOGFILE
}

# -------------------------------
# Get external files

while [[ $# -gt 0 ]]; do
  case $1 in
  -h|--help)
    echo "_PROGRAM [-options] [run files]"
    echo ""
    echo "DESCRIPTION:"
    echo " Simple shell script for automatically running GMX simulations."
    echo ""
    echo "OPTIONS:"
    echo " -f     -- MDP file path. (${MDP_PATH})"
    echo " -c     -- Structure file. (${CONF})"
    echo " -p     -- Topology file. (${TOPOL})"
    echo " -log   -- Name of logfile. (${LOGFILE})"
    echo ""
    echo "Default values (can be set with ENV):"
    echo " GMX          =" ${GMX}
    echo " GMX_FLAGS    =" ${GMX_FLAGS}
    echo ""
    echo "ABOUT:"
    echo " _PROGRAM v_VERSION -- _DATE"
    echo ""
    echo "Copyright (C) 2019 Jure Cerar"
    echo " This is free software; see the source for copying conditions.  There is NO"
    echo " warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE."
    echo ""
    exit 0
    ;;
  --version)
    echo " _PROGRAM - v_VERSION"
    exit 0
    ;;
  # MDP file path
  -f)
    shift
    MDP_PATH=$1
    shift
    ;;
  # Configuration
  -c)
    shift
    CONF=$1
    shift
    ;;
  # Topology file
  -p)
    shift
    TOPOL=$1
    shift
    ;;
  # Log file
  -log)
    shift
    LOGFILE=$1
    shift
    ;;
  # Default is RUN file
  *)
    # Construct a list
    MDP+=($1)
    shift
    ;;
  esac
done

# ----------------------------------------
# Transform to list and remove ending if present
MDP=(${MDP[@]%%.*})

# Check if any mdp files are present
if [[ -z ${MDP[@]} ]]; then
  error "No run files were provided."
  echo "Try '-h' option for more information." >&2
  exit 1
fi

# Check if file exist
for file in ${MDP[@]}; do
  if [[ ! -e ${MDP_PATH}${file}.mdp ]]; then
    error "File '${file}.mdp' could be found."
    exit 1
  fi
done

# ----------------------------------------
# Write relevant info to LOG
write "------------------------------------"
write "_PROGRAM -- $(date +"%d %b %Y %H:%M:%S")"
write "GMX       =" ${GMX}
write "GMX_FLAGS =" ${GMX_FLAGS}
write "TOPOL     =" ${TOPOL}
write "CONF      =" ${CONF}
write "MDP       =" ${MDP[@]}
write "LOGFILE   =" ${LOGFILE}

# ----------------------------------------
# Actual MD run

for RUN in ${MDP[@]}; do

  # Name of mdp file
  MDP_FILE=${MDP_PATH}${RUN}

  # Translate input files
  ${GMX} grompp -f ${MDP_FILE} -c ${CONF} -p ${TOPOL} -o ${RUN} -maxwarn 2 ${GMX_FLAGS} >/dev/null 2>&1
  if [[ $? -ne 0 ]]; then
    # Run command again to see the error
    >&2 echo -e '\033[1;91m'${GMX} grompp ERROR:'\033[0m'
    ${GMX} grompp -f ${MDP_FILE} -c ${CONF} -p ${TOPOL} -o ${RUN} -maxwarn 2 ${GMX_FLAGS} >/dev/null
    exit 1
  fi

  # Echo some basic informations
  write ""
  write "mdrun START -- $(date +"%d %b %Y %H:%M:%S")"
  write "RUN     =" ${RUN}
  write "DIR     =" $(pwd)
  write "COMMAND =" ${GMX} mdrun -deffnm ${RUN} ${GMX_FLAGS}
  write ""

  # MD run
  ${GMX} mdrun -deffnm ${RUN} ${GMX_FLAGS} 1>/dev/null || exit 0

  # Move to next
  CONF=${RUN}

done

# ----------------------------------------
write ""
write "END -- $(date +"%d %b %Y %H:%M:%S")"
write "------------------------------------"

exit 0
