#!/usr/bin/env bash

# 带颜色输出
# color red hello_world
function color() {
    case $1 in
    "black") printf "\033[30m$2\033[0m" ;;
    "red") printf "\033[31m$2\033[0m" ;;
    "green") printf "\033[32m$2\033[0m" ;;
    "yellow") printf "\033[33m$2\033[0m" ;;
    "blue") printf "\033[34m$2\033[0m" ;;
    "purple") printf "\033[35m$2\033[0m" ;;
    "sky_blue") printf "\033[36m$2\033[0m" ;;
    "white") printf "\033[37m$2\033[0m" ;;
    *) printf "$2" ;;
    esac
}

# 获取脚本真实路径，如果有链接，会取到源文件路径
# get_real_path
function get_real_path() {
#    local tar_file=${BASH_SOURCE[0]}
    local tar_file=$0
    cd $(dirname $tar_file})
    tar_file=$(basename $tar_file)
    while [ -L "$tar_file" ]; do
        tar_file=$(readlink $tar_file)
        cd $(dirname $tar_file)
        tar_file=$(basename $tar_file)
    done
    echo "$(pwd -P)/$tar_file"
}

# 获取脚本真实目录，如果有链接，会取到源文件目录
# get_real_dir
function get_real_dir() {
    echo $(dirname $(get_real_path))
}

ROOT=$(get_real_dir)
echo -e "项目路径:$(color green $ROOT)"

# 要生成头文件路径
GEN_REC_INC_PATH="example/include"
# 生成目标文件路径
GEN_REC_OUT_PATH="example/src"

echo -e "要生成头文件路径:$(color green ${GEN_REC_INC_PATH})"
echo -e "生成目标文件路径:$(color green ${GEN_REC_OUT_PATH})"

# 编译源文件
function make() {
    echo -e "$(color green "开始编译gen_rec文件")"
    cd $ROOT
    if [[ ! -d "ebin" ]]; then
        mkdir "ebin"
    fi
    erl -make
    echo -e "$(color green "编译gen_rec文件完成")"
}

# 生成record数据文件
function gen() {
    make
    cd $ROOT
    cp -r "${ROOT}/rec/"*.erl "${GEN_REC_OUT_PATH}"
    cp -r "${ROOT}/rec/"*.hrl "${GEN_REC_INC_PATH}"
    echo -e "$(color green "开始record数据文件")"
    erl -noshell -pa ebin -I inc -eval "gen_rec:main(\"${GEN_REC_INC_PATH}\",\"${GEN_REC_OUT_PATH}\")" -s init stop
    if [ $? -ne 0 ]; then
        echo -e "$(color red "生协record数据文件失败")"
        return 1
    fi
    echo -e "$(color green "生成record数据文件完成")"
}

# 帮助
function help() {
    echo -e "$(color green "请输入以下指令：")"
    printf "%-25s%s\n" "$(color green "make")" "$(color sky_blue "编译源文件")"
    printf "%-25s%s\n" "$(color green "gen")" "$(color sky_blue "生成record数据文件")"
}

case $1 in
    make)
        make
    ;;
    gen)
        gen
    ;;
    *)
        help
    ;;
esac