#!/bin/sh

if test -t 1
then
    colors=$(tput colors)
    if test -n "$colors" && test $colors -ge 8
    then
        reset="$(tput sgr0)"
        red="$(tput setaf 1)"
        green="$(tput setaf 2)"
    fi
fi

lacc="$1"
if [ -z "$lacc" ]
then
	lacc=../bin/lacc
	command -v $lacc >/dev/null 2>&1 || {
		echo "$lacc required, run 'make'."
		exit 1
	}
fi

if [ ! -f sqlite/shell.c ] || [ ! -f sqlite/sqlite3.c ]
then
	echo "Missing sqlite source, download and place in 'test/sqlite' folder."
	exit 1
fi

bin=../bin/test/sqlite
mkdir -p $bin

# Build with lacc
valgrind --leak-check=full --show-leak-kinds=all \
	$lacc -std=c89 -fPIC -g -v -o $bin/program \
		sqlite/shell.c sqlite/sqlite3.c \
		-DSQLITE_DEBUG \
		-DSQLITE_MEMDEBUG \
		--dump-symbols \
		--dump-types \
		-lm -lpthread -ldl \
		> /dev/null

if [ $? -ne 0 ]
then
	echo "${red}Compilation failed!${reset}";
	exit 1
fi

rm shell.o sqlite3.o

# Build with reference compiler
cc sqlite/shell.c sqlite/sqlite3.c -o $bin/reference -lm -lpthread -ldl

# Test case
input=$(cat <<EOF
create table tbl1(one varchar(10), two smallint);
insert into tbl1 values('hello!', 10);
insert into tbl1 values('goodbye', 20);
select * from tbl1;
EOF
)

expected=$(echo "$input" | $bin/reference)
actual=$(echo "$input" | $bin/program)

if [ "$expected" != "$actual" ]
then
	echo "${red}Wrong output!${reset}";
	exit 1
fi

echo "${green}Ok!${reset}"
exit 0
