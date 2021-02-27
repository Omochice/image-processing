function convert_pnm() {
    filename=$1
    ext=".pnm"
    dst=${filename%.*}$ext
    tmp=".tmp"$ext
    command convert $filename -compress none $tmp
    command sed "/^#\w*/d" $tmp >$dst
    command rm $tmp
}

for arg; do
    convert_pnm $arg
done
