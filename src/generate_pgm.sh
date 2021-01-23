function convert_pgm() {
    filename=$1
    dst=${filename%.*}.pgm
    command convert $filename -compress none .tmp.pgm
    command sed "/^#\w*/d" .tmp.pgm >$dst
    command rm .tmp.pgm
}

for arg; do
    convert_pgm $arg
done
