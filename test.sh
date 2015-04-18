Paths="1.png 2.png img/3.png"
for p in $Paths; do
	wget --quiet -O tmpfile http://localhost:3000/$p
	hash1=`md5sum ${HOME}/eamonn_server/${p} | awk '{ print $1 }'`
	hash2=`md5sum tmpfile | awk '{ print $1 }'`
	rm tmpfile
	if [ "$hash1" != "$hash2" ]; then
 		echo "Failed test "$p && exit 1
 	fi
 	echo "Picture $p downloaded correctly"
done
exit 0