function convert_pgm() {
    filename=$1
    ext=".pgm"
    dst=${filename%.*}$ext
    tmp=".tmp"$ext
    command convert $filename -compress none $tmp
    command sed "/^#\w*/d" $tmp >$dst
    command rm $tmp
}

for arg; do
    convert_pgm $arg
done
