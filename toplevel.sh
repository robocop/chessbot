#!/bin/bash

rm debug/*.ml
cp color.sh debug/
cp -r cursor/ debug/
sed -e 's/\(^open .*\)/(* \1 *)/' board.ml > debug/board.ml
sed -e 's/\(^open .*\)/(* \1 *)/' chess.ml > debug/chess.ml
sed -e 's/\(^open .*\)/(* \1 *)/' aux.ml > debug/aux.ml
sed -e 's/\(^open .*\)/(* \1 *)/' opens.ml > debug/opens.ml
cd debug/
ocaml -init ../.ocamlinit