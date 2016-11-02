# Run a command when a watched file changes.
#
# $1 is a command that generates a list of file paths to watched
# $2 is the command to run when a file on the watch paths changed
#
# Examples:
#
#   run_on_change.sh 'find . -name "*.erl"' 'make compile test'
#    run compile and test every time a erl file changes

#   run_on_change.sh 'echo arch.dot' 'dot -Tpng -o arch.png arch.dot'
#    create arch.png every time the dot file changes

#!/bin/bash
POLL_TIME_SECS=1
FIND_WATCHED_FILES=$1
CMD=$2

function watched_files_md5sum {
    MD5SUM=$(bash -c "$FIND_WATCHED_FILES" | xargs cat | md5sum)
    echo $MD5SUM
}

#CUR_MD5=$(watched_files_md5sum)
NEW_MD5=$(watched_files_md5sum)

I=0

while(true)
do
    if [[ $CUR_MD5 != $NEW_MD5 ]]
    then
        ((I++))
        echo "$(date) watched files changed running >>> $CMD <<<"
        $CMD
    fi
    CUR_MD5=$NEW_MD5
    NEW_MD5=$(watched_files_md5sum)
    sleep $POLL_TIME_SECS
done
