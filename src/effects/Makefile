OCAML_DIR=/home/fabrice/Work/ocaml-tsan
OCAMLOPT=${OCAML_DIR}/ocamlopt.opt
TSAN_VERBOSE_DIR=/home/fabrice/Work/playbook/src/tsan

SRC=effects.ml
BIN=effects.exe


${BIN}: ${SRC}
	${OCAMLOPT} -I ${OCAML_DIR}/stdlib -I ${OCAML_DIR}/runtime -runtime-variant d -g -ccopt -fsanitize=thread -ccopt -O1 -ccopt -g ${?} -o ${@}

out.txt: ${BIN}
	LD_PRELOAD=${TSAN_VERBOSE_DIR}/tsan_verbose.so ./${<} >${@} 2>&1

out_filtered.txt: out.txt
	grep -e "from camlExn_in_callback\." -e "^[^\[]" -e 'forced' ${<} >${@}
