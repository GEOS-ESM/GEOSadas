
dir="./temp_tobe_deleted"

if [ -d $dir ]; then
  echo "Note:  $dir  exist !"
  echo "remove  $dir"
  exit
fi

mkdir -p $dir
mv @env  @cmake   ./src/GMAO*  ./temp/.
