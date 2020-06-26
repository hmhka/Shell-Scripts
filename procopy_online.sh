SourceDB=$1
TargetDB=$2
PipeFile=/tmp/fifo.$$
mkfifo $PipeFile
probkup online $SourceDB $PipeFile -com > /dev/null 2>&1 &
prorest $TargetDB $PipeFile
rm $PipeFile

