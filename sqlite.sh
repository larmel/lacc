#!/bin/sh

lacc="$1"
comp="$2"
if [ -z "$comp" ]
then
	echo "Usage: $0 <compiler to test> <reference compiler>";
	exit 1
fi

if [ ! -f sqlite/shell.c ] || [ ! -f sqlite/sqlite3.c ]
then
	echo "Missing sqlite source, download and place in 'sqlite' folder"
	exit 1
fi

# Build with lacc
valgrind --leak-check=full --show-leak-kinds=all \
	$lacc -std=c89 -fPIC -g -v -o bin/sqlite \
		sqlite/shell.c sqlite/sqlite3.c \
		-DSQLITE_DEBUG \
		-DSQLITE_MEMDEBUG \
		--dump-symbols \
		--dump-types \
		-lm -lpthread -ldl \
		> /dev/null

if [ $? -ne 0 ]
then
	echo "$(tput setaf 1)Compilation failed!$(tput sgr0)";
	exit 1
fi

# Build with reference compiler
$comp sqlite/shell.c sqlite/sqlite3.c -o bin/sqlite-cc -lm -lpthread -ldl

# Test case
input=$(cat <<EOF
create table tbl1(one varchar(10), two smallint);
insert into tbl1 values('hello!', 10);
insert into tbl1 values('goodbye', 20);
select * from tbl1;
EOF
)

expected=$(echo "$input" | bin/sqlite-cc)
actual=$(echo "$input" | bin/sqlite)

if [ "$expected" != "$actual" ]
then
	echo "$(tput setaf 1)Wrong output!$(tput sgr0)";
	exit 1
fi

echo "$(tput setaf 2)Ok!$(tput sgr0)"
exit 0
