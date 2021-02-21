for file in *.R; do
	sed "s/trials/items/" "$file" | echo > "$file"
done