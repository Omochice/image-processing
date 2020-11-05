#!/usr/bin/env bash

function generate_portrait() {
    script_dir=$(
        cd $(dirname $0)
        pwd
    )
    json_path=$script_dir/config.json
    if ! [ -e $json_path ]; then
        echo "json is not exists."
        echo '{ "img_path" : "<Relatice path from this json>", "img_width" : "<FILL_IN>" , "img_height" : "<FILL_IN>"}' | jq . >$json_path
        echo "Created config.json. Edit it and run this again."
        return 1
    fi
    # jsonから読み取ってfortranにコマンドライン引数で渡したい
    echo "hello"

}

generate_portrait
