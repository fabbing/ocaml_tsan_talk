OCAML_DIR=/home/fabrice/Work/ocaml-tsan
OCAMLOPT=${OCAML_DIR}/ocamlopt.opt
TSAN_VERBOSE_DIR=/home/fabrice/Work/playbook/src/tsan


exn.exe: exn_in_callback.ml callbacks.c
	${OCAMLOPT} -I ${OCAML_DIR}/stdlib -I ${OCAML_DIR}/runtime -I ${OCAML_DIR}/otherlibs/unix -runtime-variant d -g -ccopt -fsanitize=thread -ccopt -O1 -ccopt -g unix.cmxa callbacks.c exn_in_callback.ml -o ${@}

out.txt: exn.exe
	LD_PRELOAD=${TSAN_VERBOSE_DIR}/tsan_verbose.so ./${<} >${@} 2>&1

out_filtered.txt: out.txt
	grep -e "from camlExn_in_callback\." -e "^[^\[]" -e 'forced' ${<} >${@}
