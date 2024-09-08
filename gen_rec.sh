#!/usr/bin/env bash

if [[ -x "func.sh" ]]; then
    source func.sh
else
    echo "找不到func.sh脚本"
    exit 1
fi

ROOT=$(get_real_dir)
echo -e "项目路径:$(color green $ROOT)"

# 要生成头文件路径
GEN_REC_INC_PATH="../server/include"
# 生成目标文件路径
GEN_REC_OUT_PATH="../server/src/rec"

echo -e "要生成头文件路径:$(color green ${GEN_REC_INC_PATH})"
echo -e "生成目标文件路径:$(color green ${GEN_REC_OUT_PATH})"

# 编译源文件
function make() {
    echo -e "$(color green "开始编译gen_rec文件")"
    if [[ ! -d "ebin" ]]; then
        mkdir "ebin"
    fi
    erl -make
    echo -e "$(color green "编译gen_rec文件完成")"
}

# 生成record数据文件
function gen_rec() {
    make
    if [[ ! -e "${GEN_REC_OUT_PATH}/rec_term.erl" ]]; then
        echo -e "$(color green "rec_term.erl不存在，复制rec_term.erl到目标文件路径")"
        cp "${ROOT}/rec/rec_term.erl" "${GEN_REC_OUT_PATH}/rec_term.erl"
    fi
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
    printf "%-25s%s\n" "$(color green "gen_rec")" "$(color sky_blue "生成record数据文件")"
}

case $1 in
    make)
        make
    ;;
    gen_rec)
        gen_rec
    ;;
    *)
        help
    ;;
esac