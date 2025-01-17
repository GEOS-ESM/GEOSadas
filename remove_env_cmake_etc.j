
dir="temp_tobe_deleted"
[[ -d $dir ]] && echo "$dir: ex" || echo "$dir: nonexist"

dir2=$HOME/trash/$dir
mydate=$(date '+%Y-%m-%d-%H-%M-%S')
dir3=$HOME/trash/${dir}_$mydate
    
if [ -d $dir ]; then
  if [ $# -eq 0 ]; then
    echo "?: remove  ./$dir"
    exit
  elif [ $1 -eq 1 ]; then
    [[ -d  $dir2 ]] && mv $dir2 $dir3
    echo "mv $dir $dir2"
    mv $dir $dir2
  fi
fi

mkdir $dir
mv @env  @cmake   ./src/GMAO*  ./$dir/.
