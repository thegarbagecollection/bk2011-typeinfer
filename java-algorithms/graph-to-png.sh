for FILE in `find . -name "*.dotgraph"`
do
    WRITEFILE=$(echo $FILE | sed "s/dotgraph/png/")
    dot -Tpng "$FILE" > "$WRITEFILE"
done