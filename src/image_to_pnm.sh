function img_to_pnm() {
    local source_dir="$(cd "$( dirname "${BASH_SOURCE[0]}" )" > /dev/null 2>&1 && pwd)"
    local prj_dir=$(dirname $source_dir)
    for input_img in $(ls  $prj_dir/img -I *.pnm); do
        local filename=$(echo $input_img | sed -e 's@\.[^\.]*$@@')
        command convert $prj_dir/img/$input_img $prj_dir/img/$filename.pnm
    done

    # input_img=$1
    # filename=$(basename $input_img  | sed "s@\.[\.]*$@@")
    # command convert $filename pnm:- > $filename.pnm
}

img_to_pnm
