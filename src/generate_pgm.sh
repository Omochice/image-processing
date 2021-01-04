function convert_pgm() {
    filename=$1
    dst=${filename%.*}.pgm
    command convert $filename -compress none $dst
}

for arg; do
    convert_pgm $arg
done
