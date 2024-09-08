#!/usr/bin/env bash
# 通用方法

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
