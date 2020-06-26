# lkinfo - script to check the contents of .lk file

lkFile=$db.lk

GetLong()
{
# Returns 4-byte unsigned integer.
  File=$1
  Offset=$2

# od -D  Interpret long words in unsigned decimal:
  od -D $File +${Offset}. | \
  awk '{ printf"%d",$2; exit }'
} # GetLong()

# Database mode:
lkMode=`GetLong $lkFile 0`
case $lkMode in
  1) Desc="Single-user mode";;
  2) Desc="Multi-user mode" ;;
 64) Desc="Crash recovery"  ;;
  *) Desc="Unknown mode"    ;;
esac

# ID of the process that opens database:
lkPID=`GetLong $lkFile 4`

# Host where the process is running:
lkHost=`tail +9c $lkFile`

echo "Database mode: $lkMode ($Desc)"
echo "Opened by PID: $lkPID"
echo "      On Host: $lkHost"
echo "    This host: `uname -n`"
ps -fp $lkPID || echo "Process PID=$lkPID not found."

